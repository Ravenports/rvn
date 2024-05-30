--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Pkgtypes;
private with Raven.Database.Schema;
private with Archive.Unpack;
private with SQLite;

package Raven.Database.Pkgs is

   --  Upon file path conflict, this will rollback unless "forced" is set.
   function rdb_register_package
     (db  : in out RDB_Connection;
      pkg : in out Pkgtypes.A_Package;
      forced : Boolean) return Boolean;

private

   package SCH renames Raven.Database.Schema;

   internal_srcfile : constant String := "raven-database-pkgs.adb";

   procedure squawk_reset_error (stmt_type : SCH.prepared_statement);

   --  Inserts stump-level package data, returns True on success
   function run_prstmt_main_pkg  (db : RDB_Connection; pkg : Pkgtypes.A_Package) return Boolean;
   function run_prstmt_category  (db : RDB_Connection; pkg : Pkgtypes.A_Package) return Boolean;
   function run_prstmt_license   (db : RDB_Connection; pkg : Pkgtypes.A_Package) return Boolean;
   function run_prstmt_user      (db : RDB_Connection; pkg : Pkgtypes.A_Package) return Boolean;
   function run_prstmt_group     (db : RDB_Connection; pkg : Pkgtypes.A_Package) return Boolean;
   function run_prstmt_directory (db : RDB_Connection; pkg : Pkgtypes.A_Package) return Boolean;
   function run_prstmt_script    (db : RDB_Connection; pkg : Pkgtypes.A_Package) return Boolean;
   function run_prstmt_library   (db : RDB_Connection; pkg : Pkgtypes.A_Package) return Boolean;
   function run_prstmt_note      (db : RDB_Connection; pkg : Pkgtypes.A_Package) return Boolean;
   function run_prstmt_option    (db : RDB_Connection; pkg : Pkgtypes.A_Package) return Boolean;
   function run_prstmt_depend    (db : RDB_Connection; pkg : Pkgtypes.A_Package) return Boolean;
   function run_prstmt_file      (db : RDB_Connection; pkg : Pkgtypes.A_Package) return Boolean;
   function run_prstmt_message   (db : RDB_Connection; pkg : Pkgtypes.A_Package) return Boolean;

   function overwrite_main_pkg
     (db    : RDB_Connection;
      pkgid : Pkgtypes.Package_ID;
      pkg   : Pkgtypes.A_Package) return Boolean;

   function delete_satellite
     (db    : RDB_Connection;
      pkgid : Pkgtypes.Package_ID;
      table : String) return Boolean;

   --  iterate through old files.  Any file not present in pkg.files will be physically deleted.
   procedure remove_orphaned_files
     (pkg   : Pkgtypes.A_Package;
      old   : Archive.Unpack.file_records.Vector);

   function overwrite_file
     (db    : RDB_Connection;
      pkgid : Pkgtypes.Package_ID;
      path  : String;
      b3sum : String) return Boolean;

end Raven.Database.Pkgs;
