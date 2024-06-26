--  SPDX-License-Identifier: ISC
--  Reference: /License.txt


with Ada.Text_IO;
with Raven.Event;
with Raven.Strings;
with Raven.Context;
with Raven.Metadata;
with Raven.Pkgtypes;
with Raven.Database.Lock;
with Raven.Database.Query;
with Raven.Database.Operations;
with Raven.Cmd.Unset;
with Archive.Dirent.Scan;
with Archive.Unix;

package body Raven.Cmd.Clean is

   package LOK renames Raven.Database.Lock;
   package QRY renames Raven.Database.Query;
   package OPS renames Raven.Database.Operations;
   package RCU renames Raven.Cmd.Unset;
   package SCN renames Archive.Dirent.Scan;

   -----------------------------
   --  execute_clean_command  --
   -----------------------------
   function execute_clean_command (comline : Cldata) return Boolean
   is
      procedure check_dirent (Position : SCN.dscan_crate.Cursor);
      procedure delete_file (Position : SCN.dscan_crate.Cursor);
      procedure find_dangling_symlink (Position : SCN.dscan_crate.Cursor);

      cachedir       : constant String := Context.reveal_cache_directory;
      catalog_map    : Pkgtypes.NV_Pairs.Map;
      cache_contents : SCN.dscan_crate.Vector;
      purge_list     : SCN.dscan_crate.Vector;
      fileattr       : Archive.Unix.File_Characteristics;
      bytes_to_purge : Archive.exabytes := 0;
      cont           : Character;
      rdb            : Database.RDB_Connection;

      procedure check_dirent (Position : SCN.dscan_crate.Cursor)
      is
         --  First pass: ignore all types except regular files
         --  If "--all" given, mark all files (even those without rvn extension) for deletion
         entity_path : constant String :=
           Archive.Dirent.full_path (SCN.dscan_crate.Element (Position));
         filename : constant String :=
                    Archive.Dirent.simple_name (SCN.dscan_crate.Element (Position));
         fileattr2 : Archive.Unix.File_Characteristics;
         use type Archive.exabytes;
      begin
         fileattr2 := Archive.Unix.get_charactistics (entity_path);
         case fileattr2.ftype is
            when Archive.regular => null;
            when others => return;
         end case;
         if comline.cmd_clean.delete_all then
            purge_list.Append (SCN.dscan_crate.Element (Position));
            bytes_to_purge := bytes_to_purge + fileattr2.size;
            if not comline.common_options.quiet then
               if comline.common_options.dry_run then
                  Event.emit_message ("dry-run: delete " & filename);
               else
                  Event.emit_message ("delete " & filename);
               end if;
            end if;
            return;
         end if;
         if entity_path'Length > 22 then
            if Strings.trails (entity_path, extension) then
               declare
                  D10 : constant Text := Strings.SUS
                    (entity_path (entity_path'Last - 13 .. entity_path'Last - 4));
                   kill_it : Boolean := True;
               begin
                  if catalog_map.Contains (D10) and then
                    Strings.equivalent (catalog_map.Element (D10), filename)
                  then
                     kill_it := False;
                  end if;
                  if kill_it then
                     purge_list.Append (SCN.dscan_crate.Element (Position));
                     bytes_to_purge := bytes_to_purge + fileattr2.size;
                     if not comline.common_options.quiet then
                        if comline.common_options.dry_run then
                           Event.emit_message ("dry-run, obsolete: " & filename);
                        else
                           Event.emit_message ("obsolete: " & filename);
                        end if;
                     end if;
                  end if;
               end;
            end if;
         end if;
      end check_dirent;

      procedure delete_file (Position : SCN.dscan_crate.Cursor)
      is
         entity_path : constant String :=
           Archive.Dirent.full_path (SCN.dscan_crate.Element (Position));
      begin
         if not Archive.Unix.unlink_file (entity_path) then
            Event.emit_error ("Failed to delete " & entity_path);
         end if;
      end delete_file;

      procedure find_dangling_symlink (Position : SCN.dscan_crate.Cursor)
      is
         entity_path : constant String :=
           Archive.Dirent.full_path (SCN.dscan_crate.Element (Position));
         parent_dir : constant String :=
           Archive.Dirent.parent_directory (SCN.dscan_crate.Element (Position)) & "/";
         fileattr2 : Archive.Unix.File_Characteristics;

         function target_path (pdir, target : String) return String is
         begin
            if target (target'First) = '/' then
               return target;
            end if;
            return parent_dir & target;
         end target_path;
      begin
         fileattr2 := Archive.Unix.get_charactistics (entity_path);
         case fileattr2.ftype is
            when Archive.symlink => null;
            when others => return;
         end case;
         declare
            raw_target : constant String := Archive.Unix.link_target (entity_path);
         begin
            if not Archive.Unix.file_exists (target_path (parent_dir, raw_target)) then
               if not Archive.Unix.unlink_file (entity_path) then
                  Event.emit_error ("Failed to delete " & entity_path);
               end if;
            end if;
         end;
      end find_dangling_symlink;

   begin
      if not Archive.Unix.file_exists (cachedir) then
         Event.emit_message ("cache directory " & cachedir & " does not exist.");
         return False;
      end if;

      fileattr := Archive.Unix.get_charactistics (cachedir);
      case fileattr.ftype is
         when Archive.directory => null;
         when others =>
            Event.emit_message ("cache " & cachedir & " is not a directory.");
            return False;
      end case;

      purge_list.Clear;
      if not OPS.localdb_exists (Database.catalog) then
         Event.emit_error
           ("Catalog database is missing, should be here: " & OPS.localdb_path (Database.catalog));
         return False;
      end if;
      case OPS.rdb_open_localdb (rdb, Database.catalog) is
         when RESULT_OK => null;
         when others => return False;
      end case;

      if not LOK.obtain_lock (rdb, LOK.lock_readonly) then
         Event.emit_error (LOK.no_read_lock);
         OPS.rdb_close (rdb);
         return False;
      end if;

      QRY.all_remote_packages (rdb, catalog_map);

      if not LOK.release_lock (rdb, LOK.lock_readonly) then
         Event.emit_error (LOK.no_read_unlock);
         OPS.rdb_close (rdb);
         return False;
      end if;

      OPS.rdb_close (rdb);
      SCN.scan_directory (cachedir, cache_contents);
      cache_contents.Iterate (check_dirent'Access);
      if purge_list.Is_Empty then
         if not comline.common_options.quiet then
            Event.emit_message ("No files were selected for deletion.");
            return True;
         end if;
      end if;
      if not comline.common_options.quiet and then RCU.config_setting (RCU.CFG.assume_yes) then
         --  Always show summary of what would happen unless --quiet and --yes are both set
         declare
            total : constant String := Metadata.human_readable_size (int64 (bytes_to_purge));
            frag  : constant String := " selected for deletion (freeing " & total & ")";
            fnum  : constant Natural := Natural (purge_list.Length);
         begin
            if fnum = 1 then
               Event.emit_message ("1 file" & frag);
            else
               Event.emit_message (Strings.int2str (fnum) & " files" & frag);
            end if;
         end;
      end if;

      if comline.common_options.dry_run then
         return True;
      end if;

      if not RCU.config_setting (RCU.CFG.assume_yes) then
         Event.emit_message ("Continue? [y/n]");
         Ada.Text_IO.Get_Immediate (cont);
         case cont is
            when 'Y' | 'y' => null;
            when others => return True;
         end case;
      end if;

      purge_list.Iterate (delete_file'Access);
      purge_list.clear;
      cache_contents.Iterate (find_dangling_symlink'Access);
      cache_contents.Clear;

      return True;
   end execute_clean_command;

end Raven.Cmd.Clean;
