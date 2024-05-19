--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Text_IO;
with Ada.Directories;
with Raven.Event;
with Raven.Context;
with Raven.Pkgtypes;
with Raven.Metadata;
with Raven.Database.Pkgs;
with Raven.Database.Operations;
with ThickUCL.Files;
with Archive.Unpack;
with Archive.Unix;

package body Raven.Catalog is

   package TIO renames Ada.Text_IO;
   package DIR renames Ada.Directories;
   package PAC renames Raven.Database.Pkgs;
   package OPS renames Raven.Database.Operations;

   -------------------------
   --  generate_database  --
   -------------------------
   function generate_database (tracker : out Natural) return Boolean
   is
      func        : constant String := "generate_database: ";
      db_path     : constant String := OPS.localdb_path (Database.catalog);
      backup_path : constant String := db_path & ".backup";
      catalog_ucl : constant String := Context.reveal_cache_directory & "/remote/catalog.ucl";
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
      exception
         when others =>
            if TIO.Is_Open (cat_handle) then
               TIO.Close (cat_handle);
            end if;
            Event.emit_error (func & "Failed during read of catalog database");
            goto cleanup_failure;
      end;

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

end Raven.Catalog;
