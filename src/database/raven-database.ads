--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

private with SQLite;

package Raven.Database is

   --  TODO: this needs to be set during initialization
   rdb_case_sensitive : Boolean := False;

   type RDB_Connection is limited private;
   type RDB_Connection_Access is access all RDB_Connection;

private

   localhost_database : constant String := "local.sqlite";

   type RDB_Connection is limited
      record
         handle             : aliased SQLite.db3 := null;
         prstmt_initialized : Boolean := False;
      end record;

end Raven.Database;
