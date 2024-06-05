--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Text_IO;
with Raven.Event;
with Raven.Metadata;
with Raven.Deinstall;
with Archive.Unpack;
with Archive.Unix;
with ThickUCL;

package body Raven.Install is

   package TIO renames Ada.Text_IO;

   function reinstall_or_upgrade (rdb         : in out Database.RDB_Connection;
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

      Deinstall.deinstall_extracted_package (installed_package   => current_pkg,
                                             verify_digest_first => False,
                                             quiet               => True,
                                             inhibit_scripts     => no_scripts,
                                             post_report         => TIO.File_Type);


      return False;

   end reinstall_or_upgrade;

end Raven.Install;
