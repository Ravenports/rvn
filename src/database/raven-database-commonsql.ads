--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with SQLite;

package Raven.Database.CommonSQL is

   procedure ERROR_SQLITE
     (db      : not null SQLite.db3;
      srcfile : String;
      func    : String;
      query   : String);

   function transaction_begin
     (db        : not null SQLite.db3;
      srcfile   : String;
      func      : String;
      savepoint : String) return Boolean;

   function transaction_commit
     (db        : not null SQLite.db3;
      srcfile   : String;
      func      : String;
      savepoint : String) return Boolean;

   function transaction_rollback
     (db        : not null SQLite.db3;
      srcfile   : String;
      func      : String;
      savepoint : String) return Boolean;

   function exec
     (db        : not null SQLite.db3;
      sql       : String) return Action_Result;

   function get_int64
     (db        : not null SQLite.db3;
      srcfile   : String;
      func      : String;
      sql       : String;
      res       : out int64;
      silence   : Boolean) return Boolean;

private

   function run_transaction
     (db        : not null SQLite.db3;
      srcfile   : String;
      func      : String;
      query     : String;
      savepoint : String) return Boolean;

end Raven.Database.CommonSQL;
