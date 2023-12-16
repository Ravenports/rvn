--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

private with SQLite;
private with Raven.Database.Schema;

package Raven.Database.Operations is

   function rdb_open_localdb (db : in out RDB_Connection) return Action_Result;
   procedure rdb_close       (db : in out RDB_Connection);

   function localdb_exists return Boolean;
   function localdb_path   return String;

private

   internal_srcfile : constant String := "raven-database-operations.adb";
   prepared_statements : array (Raven.Database.Schema.prepared_statement) of SQLite.thick_stmt;

   function establish_localhost_connection (db : in out RDB_Connection) return Action_Result;

   --  Returns true on success
   function create_localhost_database (db : in out RDB_Connection) return Boolean;

   --  Return true on success (OK) otherwise false (FATAL)
   function initialize_prepared_statements (db : in out RDB_Connection) return Boolean;

   --  Close the initialized statements
   procedure finalize_prepared_statements (db : in out RDB_Connection);

   --  Apply schema updates since the base version (OK => True, FATAL => False)
   function upgrade_schema (db : in out RDB_Connection) return Boolean;

end Raven.Database.Operations;
