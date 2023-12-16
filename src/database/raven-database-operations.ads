--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

package Raven.Database.Operations is

   procedure rdb_close
     (db : in out RDB_Connection);

private

   internal_srcfile : constant String := "raven-database-operations.adb";

   function establish_connection (db : in out RDB_Connection) return Action_Result;

   --  Returns true on success
   function create_localhost_database (db : in out RDB_Connection) return Boolean;

end Raven.Database.Operations;
