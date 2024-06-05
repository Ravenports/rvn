--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Text_IO;
with Raven.Event;
with Raven.Metadata;
with Raven.Deinstall;
with Raven.Database.Pkgs;
with Archive.Unpack;
with Archive.Unix;
with ThickUCL;

package body Raven.Install is

   package TIO  renames Ada.Text_IO;
   package PKGS renames Raven.Database.Pkgs;

   function reinstall_or_upgrade (rdb         : in out Database.RDB_Connection;
                                  action      : refresh_action;
                                  current_pkg : Pkgtypes.A_Package;
                                  updated_pkg : String;
                                  rootdir     : String;
                                  no_scripts  : Boolean;
                                  post_report : TIO.File_Type) return Boolean
   is
      features  : Archive.Unix.File_Characteristics;
      file_list : Archive.Unpack.file_records.Vector;
      operation : Archive.Unpack.Darc;
      metatree  : ThickUCL.UclTree;
      shiny_pkg : Pkgtypes.A_Package;
      success   : Boolean;

      function extract_location return String is
      begin
         if rootdir = "" then
            return "/";
         end if;
         return rootdir;
      end extract_location;

      procedure transfer_custom_notes (Position : Pkgtypes.NoteSet.Cursor)
      is
         mynote : Pkgtypes.Note_Item renames Pkgtypes.NoteSet.Element (Position);
      begin
         if mynote.custom then
            shiny_pkg.annotations.Insert (mynote.tag, mynote);
         end if;
      end transfer_custom_notes;
   begin
      features := Archive.Unix.get_charactistics (updated_pkg);
      case features.ftype is
         when Archive.regular => null;
         when Archive.unsupported =>
            Event.emit_error ("Dev error: " & updated_pkg & " package D.N.E.");
            return False;
         when others =>
            Event.emit_error ("Not a regular file: " & updated_pkg);
            return False;
      end case;

      operation.open_rvn_archive (updated_pkg, Archive.silent, Archive.Unix.not_connected);
      if not operation.extract_manifest (file_list, extract_location) then
         Event.emit_error ("Failed to extract manifest of packaged files.");
      end if;
      operation.populate_metadata_tree (metatree);
      operation.close_rvn_archive;

      Metadata.convert_to_package (metatree, file_list, shiny_pkg, current_pkg.automatic);
      current_pkg.annotations.Iterate (transfer_custom_notes'Access);

      case action is
         when upgrade =>
            Deinstall.deinstall_extracted_package (installed_package   => current_pkg,
                                                   verify_digest_first => False,
                                                   quiet               => True,
                                                   inhibit_scripts     => no_scripts,
                                                   post_report         => TIO.File_Type);

            success := PKGS.rdb_register_package (db     => rdb,
                                                  pkg    => shiny_pkg,
                                                  forced => False);
            if success then
               success := install_files_from_archive (archive_path    => updated_pkg,
                                                      root_directory  => extract_location,
                                                      inhibit_scripts => no_scripts,
                                                      be_silent       => True,
                                                      dry_run_only    => False,
                                                      upgrading       => True);
            end if;
         when reinstall =>
            success := PKGS.rdb_register_package (db     => rdb,
                                                  pkg    => shiny_pkg,
                                                  forced => True);
            if success then
               success := install_files_from_archive (archive_path    => updated_pkg,
                                                      root_directory  => extract_location,
                                                      inhibit_scripts => no_scripts,
                                                      be_silent       => True,
                                                      dry_run_only    => False,
                                                      upgrading       => False);
               end if;
            end case;

      return success;

   end reinstall_or_upgrade;

end Raven.Install;
