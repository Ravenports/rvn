--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Event;
with Raven.Strings; use Raven.Strings;

package body Raven.Database.CommonSQL is

   --------------------
   --  ERROR_SQLITE  --
   --------------------
   procedure ERROR_SQLITE (db      : not null SQLite.db3;
                           srcfile : String;
                           func    : String;
                           query   : String)
   is
      msg : String := "sqlite error while executing " & query &
        " in file " & srcfile & ", " & func & ": " &
        SQLite.get_last_error_message (db);
   begin
      Event.emit_error (msg);
   end ERROR_SQLITE;


   -------------------------
   --  transaction_begin  --
   -------------------------
   function transaction_begin
     (db        : not null SQLite.db3;
      srcfile   : String;
      func      : String;
      savepoint : String) return Boolean is
   begin
      if IsBlank (savepoint) then
         return run_transaction (db, srcfile, func, "BEGIN IMMEDIATE TRANSACTION", "");
      else
         return run_transaction (db, srcfile, func, "SAVEPOINT", savepoint);
      end if;
   end transaction_begin;


   --------------------------
   --  transaction_commit  --
   --------------------------
   function transaction_commit
     (db        : not null SQLite.db3;
      srcfile   : String;
      func      : String;
      savepoint : String) return Boolean is
   begin
      if IsBlank (savepoint) then
         return run_transaction (db, srcfile, func, "COMMIT TRANSACTION", "");
      else
         return run_transaction (db, srcfile, func, "RELEASE SAVEPOINT", savepoint);
      end if;
   end transaction_commit;


   ----------------------------
   --  transaction_rollback  --
   ----------------------------
   function transaction_rollback
     (db        : not null SQLite.db3;
      srcfile   : String;
      func      : String;
      savepoint : String) return Boolean is
   begin
      if IsBlank (savepoint) then
         return run_transaction (db, srcfile, func, "ROLLBACK TRANSACTION", "");
      else
         return run_transaction (db, srcfile, func, "ROLLBACK TO SAVEPOINT", savepoint);
      end if;
   end transaction_rollback;


   ------------
   --  exec  --
   ------------
   function exec
     (db        : not null SQLite.db3;
      sql       : String) return Action_Result is
   begin
      Event.emit_debug (high_level, "executing " & SQ (sql));
      if SQLite.exec_sql (db, sql) then
         return RESULT_OK;
      else
         Event.emit_error ("CommonSQL.exec() error: " & SQLite.last_exec_message);
         return RESULT_FATAL;
      end if;
   end exec;


   -----------------------
   --  run_transaction  --
   -----------------------
   function run_transaction (db        : not null SQLite.db3;
                             srcfile   : String;
                             func      : String;
                             query     : String;
                             savepoint : String) return Boolean
   is
      function joinsql return String is
      begin
         if IsBlank (savepoint) then
            return query;
         else
            return query & " " & savepoint;
         end if;
      end joinsql;

      stmt : SQLite.thick_stmt;
   begin
      Event.emit_debug (high_level, "RDB: running " & DQ (joinsql));
      if SQLite.prepare_sql (db, joinsql, stmt) then
         if not SQLite.step_to_completion (stmt => stmt, num_retries => 6) then
            ERROR_SQLITE (db, srcfile, func, joinsql);
            SQLite.finalize_statement (stmt);
            return False;
         end if;
         SQLite.finalize_statement (stmt);
         return True;
      else
         ERROR_SQLITE (db, srcfile, func, joinsql);
         return False;
      end if;
   end run_transaction;


end Raven.Database.CommonSQL;
