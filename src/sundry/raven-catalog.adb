--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Text_IO;
with Ada.Directories;
with Raven.Event;
with Raven.Context;
with Raven.Strings;
with Raven.Pkgtypes;
with Raven.Metadata;
with Raven.Database.Pkgs;
with Raven.Database.Lock;
with Raven.Database.Query;
with Raven.Database.Operations;
with ThickUCL.Files;
with Archive.Dirent.Scan;
with Archive.Unpack;
with Archive.Unix;

package body Raven.Catalog is

   package TIO renames Ada.Text_IO;
   package DIR renames Ada.Directories;
   package LOK renames Raven.Database.Lock;
   package PAC renames Raven.Database.Pkgs;
   package OPS renames Raven.Database.Operations;
   package QRY renames Raven.Database.Query;
   package SCN renames Archive.Dirent.Scan;

   -------------------------
   --  generate_database  --
   -------------------------
   function generate_database (tracker : out Natural) return Boolean
   is
      func        : constant String := "generate_database: ";
      db_path     : constant String := OPS.localdb_path (Database.catalog);
      backup_path : constant String := db_path & ".backup";
      catalog_ucl : constant String := Context.reveal_cache_directory & "/remote/catalog.ucl";
      srcfile     : constant String := "raven-catalog.adb";
      name_point  : constant String := "IMPORTCAT";
      backed_up   : Boolean := False;
      rvndb       : Database.RDB_Connection;
   begin
      tracker := 0;
      if not Archive.Unix.user_is_root then
         Event.emit_error (func & "Developer error, this restricted to the superuser.");
         return False;
      end if;

      if not DIR.Exists (catalog_ucl) then
         Event.emit_error (func & "Missing catalog.ucl file blocks catalog update");
         return False;
      end if;

      if OPS.localdb_exists (Database.catalog) then
         begin
            DIR.Rename (db_path, backup_path);
         exception
            when others =>
               Event.emit_error (func & "Failed to relocate " & db_path);
               return False;
         end;
         backed_up := True;
      end if;

      case OPS.rdb_open_localdb (rvndb, Database.catalog) is
         when RESULT_OK => null;
         when others =>
            Event.emit_error (func & "Failed to create catalog database");
            goto cleanup_failure;
      end case;

      if not LOK.obtain_lock (rvndb, LOK.lock_exclusive) then
         Event.emit_error (LOK.no_exc_lock);
         OPS.rdb_close (rvndb);
         goto cleanup_failure;
      end if;

      if not OPS.external_transaction_begin (rvndb, srcfile, func, name_point) then
         OPS.rdb_close (rvndb);
         goto cleanup_failure;
      end if;

      declare
         cat_handle : TIO.File_Type;
      begin
         TIO.Open (cat_handle, TIO.In_File, catalog_ucl);
         While not  TIO.End_Of_File (cat_handle) loop
            declare
               rvntree  : ThickUCL.UclTree;
               no_files : Archive.Unpack.file_records.Vector;
               new_pkg  : Pkgtypes.A_Package;
            begin
               tracker := tracker + 1;
               ThickUCL.Files.parse_ucl_string (rvntree, TIO.Get_Line (cat_handle), "");
               Metadata.convert_to_package (rvntree, no_files, new_pkg, False);
               if not PAC.rdb_register_package (rvndb, new_pkg, False) then
                  Event.emit_error (func & "catalog item insertion failed, line" & tracker'Img);
               end if;
            exception
               when ThickUCL.Files.ucl_data_unparseable =>
                  Event.emit_debug (moderate, func & "failed to parse catalog line" & tracker'Img);
            end;
         end loop;
         TIO.Close (cat_handle);
         OPS.external_transaction_commit (rvndb, srcfile, func, name_point);
      exception
         when others =>
            if TIO.Is_Open (cat_handle) then
               TIO.Close (cat_handle);
            end if;
            Event.emit_error (func & "Failed during read of catalog database");
            OPS.external_transaction_rollback (rvndb, srcfile, func, name_point);
            if not LOK.release_lock (rvndb, LOK.lock_exclusive) then
               Event.emit_error (LOK.no_exclusive_unlock);
            end if;
            OPS.rdb_close (rvndb);
            goto cleanup_failure;
      end;
      if not LOK.release_lock (rvndb, LOK.lock_exclusive) then
         Event.emit_error (LOK.no_exclusive_unlock);
         OPS.rdb_close (rvndb);
         goto cleanup_failure;
      end if;
      OPS.rdb_close (rvndb);

      if backed_up then
         begin
            DIR.Delete_File (backup_path);
         exception
            when others =>
               Event.emit_error (func & "Failed to delete backup catalog");
               goto cleanup_failure;
         end;
      end if;

      return True;

      <<cleanup_failure>>
      if backed_up then
         begin
            if OPS.localdb_exists (Database.catalog) then
               DIR.Delete_File (db_path);
            end if;
            DIR.Rename (backup_path, db_path);
         exception
            when others =>
               Event.emit_error (func & "Failed to restore backup catalog");
         end;
      end if;
      return False;

   end generate_database;


   -----------------------------------
   --  remove_obsolete_cache_links  --
   -----------------------------------
   procedure remove_obsolete_cache_links
   is
      cachedir       : constant String := Context.reveal_cache_directory;
      funcname       : constant String := "remove_obsolete_cache_links: ";
      catalog_map    : Pkgtypes.NV_Pairs.Map;
      cache_contents : SCN.dscan_crate.Vector;
      fileattr       : Archive.Unix.File_Characteristics;
      rdb            : Database.RDB_Connection;

      procedure prune_old_symlinks (Position : SCN.dscan_crate.Cursor)
      is
         --  ignore all types except symbolic links
         path : constant String := Archive.Dirent.full_path (SCN.dscan_crate.Element (Position));
         fileattr : Archive.Unix.File_Characteristics;
      begin
         fileattr := Archive.Unix.get_charactistics (path);
         case fileattr.ftype is
            when Archive.symlink => null;
            when others => return;
         end case;
         declare
            target   : constant String := Archive.Unix.link_target (path);
            filename : constant String := Strings.tail (target, "/");
            D10      : constant Text :=
                       Strings.SUS (filename (filename'Last - 13 .. filename'Last - 4));
            dangling : Boolean := False;
         begin
            fileattr := Archive.Unix.get_charactistics (target);
            case fileattr.ftype is
               when Archive.regular => null;
               when others => dangling := True;
            end case;
            if dangling or else
              not catalog_map.Contains (D10) or else
              not Strings.equivalent (catalog_map.Element (D10), filename)
            then
               Event.emit_debug
                 (high_level, funcname & "removing link to obsolete package (" & filename & ")");
               if not Archive.Unix.unlink_file (path) then
                  Event.emit_debug (high_level, funcname & "Failed to delete " & path);
               end if;
            end if;
         end;
      end prune_old_symlinks;
   begin
      fileattr := Archive.Unix.get_charactistics (cachedir);
      case fileattr.ftype is
         when Archive.directory => null;
         when others =>
            Event.emit_debug (high_level, funcname & cachedir & " DNE or is not a directory.");
            return;
      end case;

      if not OPS.localdb_exists (Database.catalog) then
         Event.emit_debug (high_level, funcname & "catalog database is missing");
         return;
      end if;
      case OPS.rdb_open_localdb (rdb, Database.catalog) is
         when RESULT_OK => null;
         when others => return;
      end case;

      if not LOK.obtain_lock (rdb, LOK.lock_readonly) then
         Event.emit_error (LOK.no_read_lock);
         OPS.rdb_close (rdb);
         return;
      end if;

      QRY.all_remote_packages (rdb, catalog_map);

      if not LOK.release_lock (rdb, LOK.lock_readonly) then
         Event.emit_error (LOK.no_read_unlock);
         OPS.rdb_close (rdb);
         return;
      end if;

      OPS.rdb_close (rdb);
      SCN.scan_directory (cachedir, cache_contents);
      cache_contents.Iterate (prune_old_symlinks'Access);

   end remove_obsolete_cache_links;


end Raven.Catalog;
