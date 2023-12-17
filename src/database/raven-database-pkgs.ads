--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Pkgtypes;
private with Raven.Database.Schema;
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
   procedure debug_running_stmt (stmt : SQLite.thick_stmt);

   --  Inserts stump-level package data, returns True on success
   function run_prstmt_main_pkg (db : RDB_Connection; pkg : Pkgtypes.A_Package) return Boolean;
   function run_prstmt_category (db : RDB_Connection; pkg : Pkgtypes.A_Package) return Boolean;
   function run_prstmt_license  (db : RDB_Connection; pkg : Pkgtypes.A_Package) return Boolean;

end Raven.Database.Pkgs;
