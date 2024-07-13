--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with SQLite;
with Raven.Database.Schema;

package Raven.Database.Operations is

   function rdb_open_localdb
     (db : in out RDB_Connection;
      contents : RDB_Contents) return Action_Result;

   procedure rdb_close
     (db : in out RDB_Connection);

   function localdb_exists (contents : RDB_Contents) return Boolean;
   function localdb_path (contents : RDB_Contents)  return String;

   rvnindex_statement  : SQLite.thick_stmt;

   function rindex_db_open
     (db           : in out RDB_Connection;
      truncate_db  : Boolean;
      index_dbdir  : String;
      index_dbname : String) return Action_Result;

   procedure rindex_db_close (db : in out RDB_Connection);

private

   internal_srcfile : constant String := "raven-database-operations.adb";

   function establish_localhost_connection
     (db : in out RDB_Connection;
      contents : RDB_Contents) return Action_Result;

   --  Returns true on success
   function create_localhost_database (db : in out RDB_Connection) return Boolean;

   --  Return true on success (OK) otherwise false (FATAL)
   function initialize_prepared_statements (db : in out RDB_Connection) return Boolean;

   --  Close the initialized statements
   procedure finalize_prepared_statements (db : in out RDB_Connection);

   --  Apply schema updates since the base version (OK => True, FATAL => False)
   function upgrade_schema (db : in out RDB_Connection) return Boolean;

   --  Returns true on success
   function create_rvnindex_database (db : in out RDB_Connection) return Boolean;

   function establish_rvn_connection
     (db           : in out RDB_Connection;
      truncate_db  : Boolean;
      index_dbdir  : String;
      index_dbname : String) return Action_Result;

   function initialize_rvnindex_statements (db : in out RDB_Connection) return Boolean;

   procedure finalize_rvnindex_statements (db : in out RDB_Connection);

end Raven.Database.Operations;
