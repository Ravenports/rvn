--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Pkgtypes;
private with Raven.Database.Schema;
private with Archive.Unpack;
private with SQLite;

use Raven.Pkgtypes;

package Raven.Database.Pkgs is

   --  Upon file path conflict, this will rollback unless "forced" is set.
   --  Conflicts are checked *before* this is run, so the rollback should not occur.
   function rdb_register_package
     (db  : in out RDB_Connection;
      pkg : in out Pkgtypes.A_Package;
      forced : Boolean) return Boolean;

   --  changes the automatic flag.
   --  This routine should only be run when the flag is different from desired.
   function rdb_reset_automatic
     (db  : in out RDB_Connection;
      pkg : in out Pkgtypes.A_Package) return Boolean;

private

   package SCH renames Raven.Database.Schema;

   internal_srcfile : constant String := "raven-database-pkgs.adb";

   procedure squawk_reset_error (stmt_type : prepared_statement);

   --  Inserts stump-level package data, returns True on success
   function run_prstmt_main_pkg  (db : in out RDB_Connection; pkg : A_Package) return Boolean;
   function run_prstmt_category  (db : in out RDB_Connection; pkg : A_Package) return Boolean;
   function run_prstmt_license   (db : in out RDB_Connection; pkg : A_Package) return Boolean;
   function run_prstmt_user      (db : in out RDB_Connection; pkg : A_Package) return Boolean;
   function run_prstmt_group     (db : in out RDB_Connection; pkg : A_Package) return Boolean;
   function run_prstmt_directory (db : in out RDB_Connection; pkg : A_Package) return Boolean;
   function run_prstmt_script    (db : in out RDB_Connection; pkg : A_Package) return Boolean;
   function run_prstmt_library   (db : in out RDB_Connection; pkg : A_Package) return Boolean;
   function run_prstmt_note      (db : in out RDB_Connection; pkg : A_Package) return Boolean;
   function run_prstmt_option    (db : in out RDB_Connection; pkg : A_Package) return Boolean;
   function run_prstmt_depend    (db : in out RDB_Connection; pkg : A_Package) return Boolean;
   function run_prstmt_file      (db : in out RDB_Connection; pkg : A_Package) return Boolean;
   function run_prstmt_message   (db : in out RDB_Connection; pkg : A_Package) return Boolean;
   function run_prstmt_trigger   (db : in out RDB_Connection; pkg : A_Package) return Boolean;

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
