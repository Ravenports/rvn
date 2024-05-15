--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

private with SQLite;

package Raven.Database is

   LOCAL_SCHEMA_VERSION : constant Natural := 1;
   type Local_Upgrade_Series is range 1 .. LOCAL_SCHEMA_VERSION;

   type RDB_Connection is limited private;
   type RDB_Connection_Access is access all RDB_Connection;
   type RDB_Contents is (installed_packages, catalog);

private

   localhost_database : constant String := "local.sqlite";
   localhost_catalog  : constant String := "catalog.sqlite";

   type RDB_Connection is limited
      record
         handle             : aliased SQLite.db3 := null;
         prstmt_initialized : Boolean := False;
      end record;

   procedure debug_running_stmt (stmt : SQLite.thick_stmt);

   --  Return localhost_database or localhost_catalog depending on content type
   function database_filename (contents : RDB_Contents) return String;

end Raven.Database;
