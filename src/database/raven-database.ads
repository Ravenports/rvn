--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

private with SQLite;

package Raven.Database is

   LOCAL_SCHEMA_VERSION : constant Natural := 3;
   type Local_Upgrade_Series is range 1 .. LOCAL_SCHEMA_VERSION;

   type RDB_Connection is limited private;
   type RDB_Connection_Access is access all RDB_Connection;
   type RDB_Contents is (installed_packages, catalog);

   type prepared_statement is
     (main_pkg, user, pkg_user, group, pkg_group, script, pkg_script, option, pkg_option,
      license, pkg_license, category, pkg_category, directory, pkg_directory, note, pkg_note,
      dependency, pkg_dependency, library, pkg_provided_lib, pkg_required_lib, pkg_adjacent_lib,
      pkg_file, pkg_message, pkg_trigger, trig_paths);

private

   localhost_database : constant String := "local.sqlite";
   localhost_catalog  : constant String := "catalog.sqlite";

   type Statement_Set is array (prepared_statement) of SQLite.thick_stmt;

   type RDB_Connection is limited
      record
         handle              : aliased SQLite.db3 := null;
         prstmt_initialized  : Boolean := False;
         prepared_statements : Statement_Set;
      end record;

   procedure debug_running_stmt (stmt : SQLite.thick_stmt);

   --  Return localhost_database or localhost_catalog depending on content type
   function database_filename (contents : RDB_Contents) return String;

end Raven.Database;
