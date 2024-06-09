--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Database.CommonSQL;
with Raven.Cmd.Unset;
with Raven.Event;

package body Raven.Database.Lock is

   package RCU renames Raven.Cmd.Unset;

   ------------------
   --  reset_lock  --
   ------------------
   function reset_lock (db : in out RDB_Connection) return Boolean
   is
      sql : constant String := "UPDATE lock_state SET exclusive=0, advisory=0, read=0;";
   begin
      case CommonSQL.exec (db.handle, sql) is
         when RESULT_OK => return True;
         when others => return False;
      end case;
   end reset_lock;


   -----------------------
   --  remove_lock_pid  --
   -----------------------
   function remove_lock_pid
     (db  : in out RDB_Connection;
      pid : Unix.Process_ID) return lock_result
   is
      func     : constant String := "remove_lock_pid";
      sql      : constant String := "DELETE FROM lock_process WHERE pid = ?1;";
      new_stmt : SQLite.thick_stmt;
      done     : lock_result;
   begin
      if not SQLite.prepare_sql (db.handle, sql, new_stmt) then
         CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func, sql);
         return lock_fatal;
      end if;
      SQLite.bind_integer (new_stmt, 1, SQLite.sql_int64 (pid));
      debug_running_stmt (new_stmt);

      case SQLite.step (new_stmt) is
         when SQLite.no_more_data =>
            done := lock_okay;
         when SQLite.row_present | SQLite.something_else =>
            CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                         SQLite.get_expanded_sql (new_stmt));
            done := lock_fatal;
      end case;
      SQLite.finalize_statement (new_stmt);
      return done;
   end remove_lock_pid;


   ----------------------
   --  check_lock_pid  --
   ----------------------
   function check_lock_pid (db : in out RDB_Connection) return lock_result
   is
      func     : constant String := "check_lock_pid";
      sql      : constant String := "SELECT pid FROM lock_process;";
      new_stmt : SQLite.thick_stmt;
      pid      : Unix.Process_ID;
      lpid     : Unix.Process_ID;
      still_locked : Boolean := False;

      use type Unix.Process_ID;
   begin
      if not SQLite.prepare_sql (db.handle, sql, new_stmt) then
         CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func, sql);
         return lock_fatal;
      end if;
      lpid := Unix.getpid;

      debug_running_stmt (new_stmt);
      loop
         case SQLite.step (new_stmt) is
            when SQLite.no_more_data =>
               exit;
            when SQLite.row_present =>
               pid := Unix.Process_ID (SQLite.retrieve_integer (new_stmt, 0));
               if pid /= lpid then
                  if not Unix.running_process (pid) then
                     Event.emit_debug (moderate, "found stale pid" & pid'Img &
                                         " in lock database, my pid is:" & lpid'Img);
                     case remove_lock_pid (db, pid) is
                        when lock_okay | lock_end => null;
                        when lock_fatal =>
                           SQLite.finalize_statement (new_stmt);
                           return lock_fatal;
                     end case;
                  else
                     Event.emit_notice ("process with pid" & pid'Img & " still holds the lock");
                     still_locked := True;
                  end if;
               end if;
            when SQLite.something_else =>
               CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                            SQLite.get_expanded_sql (new_stmt));
               SQLite.finalize_statement (new_stmt);
               return lock_fatal;
         end case;
      end loop;
      SQLite.finalize_statement (new_stmt);

      if not still_locked then
         return lock_end;
      end if;

      return lock_okay;
   end check_lock_pid;


   ----------------------
   --  write_lock_pid  --
   ----------------------
   function write_lock_pid (db : in out RDB_Connection) return boolean
   is
      func     : constant String := "write_lock_pid";
      sql      : constant String := "INSERT INTO lock_process VALUES (?1);";
      new_stmt : SQLite.thick_stmt;
      done     : Boolean := False;
   begin
      if not SQLite.prepare_sql (db.handle, sql, new_stmt) then
         CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func, sql);
         return False;
      end if;
      SQLite.bind_integer (new_stmt, 1, SQLite.sql_int64 (Unix.getpid));
      debug_running_stmt (new_stmt);

      case SQLite.step (new_stmt) is
         when SQLite.no_more_data =>
            done := True;
         when SQLite.row_present | SQLite.something_else =>
            CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                         SQLite.get_expanded_sql (new_stmt));
      end case;
      SQLite.finalize_statement (new_stmt);
      return done;
   end write_lock_pid;


   -------------------
   --  obtain_lock  --
   -------------------
   function obtain_lock
     (db       : in out RDB_Connection;
      lock     : lock_type) return Boolean
   is
      set_read_sql : constant String := "UPDATE lock_state SET read=read+1 WHERE exclusive=0;";
      set_advi_sql : constant String := "UPDATE lock_state SET advisory=1 " &
                                        "WHERE exclusive=0 AND advisory=0;";
      set_excl_sql : constant String := "UPDATE lock_state SET exclusive=1 " &
                                        "WHERE exclusive=0 AND advisory=0 AND read=0;";
      save         : constant String := "LOCKDB";
      func         : constant String := "obtain_lock";
      read_lock    : constant Boolean := RCU.config_setting (RCU.CFG.read_lock);
      locked       : Boolean;

      procedure start_trax is
      begin
         if not CommonSQL.transaction_begin (db.handle, internal_srcfile, func, save) then
            Event.emit_error (func & ": Failed to start transaction");
         end if;
      end start_trax;

      procedure abort_trax is
      begin
         if not CommonSQL.transaction_rollback (db.handle, internal_srcfile, func, save) then
            Event.emit_error (func & ": Failed to rollback transaction");
         end if;
      end abort_trax;

      procedure complete_trax is
      begin
         if not CommonSQL.transaction_commit (db.handle, internal_srcfile, func, save) then
            Event.emit_error (func & ": Failed to commit transaction");
         end if;
      end complete_trax;
   begin
      case lock is
         when lock_readonly =>
            if not read_lock then
               return True;
            end if;
            Event.emit_debug (moderate, "want to get a read only lock on a database");
            start_trax;
            locked := try_lock (db, set_read_sql, lock, False);
         when lock_advisory =>
            Event.emit_debug (moderate, "want to get an advisory lock on a database");
            start_trax;
            locked := try_lock (db, set_advi_sql, lock, False);
         when lock_exclusive =>
            Event.emit_debug (moderate, "want to get an exclusive lock on a database");
            start_trax;
            locked := try_lock (db, set_excl_sql, lock, False);
      end case;

      if locked then
         complete_trax;
      else
         Event.emit_debug (moderate, "failed to obtain the lock: " &
                             SQLite.get_last_error_message (db.handle));
         abort_trax;
      end if;

      return locked;
   end obtain_lock;


   ----------------
   --  try_lock  --
   ----------------
   function try_lock
     (db       : in out RDB_Connection;
      lock_sql : String;
      lock     : lock_type;
      upgrade  : Boolean) return Boolean
   is
      reset_state : constant String := "DELETE FROM lock_state; " &
                                       "INSERT INTO lock_state VALUES (0,0,0);";
      tries       : int64 := 0;
      timeout     : constant int64 := RCU.config_setting (RCU.CFG.lock_wait);
      max_retries : constant int64 := RCU.config_setting (RCU.CFG.lock_retries);
      locked_it   : Boolean := False;
   begin
      loop
         exit when tries > max_retries;
         case CommonSQL.exec (db.handle, lock_sql) is
            when RESULT_OK => null;
            when others => return False;
         end case;
         locked_it := False;
         if SQLite.get_number_of_changes (db.handle) = 0 then
            case check_lock_pid (db) is
               when lock_end =>
                  --  No live processes found, so we can safely reset lock
                  Event.emit_debug (moderate, "no concurrent processes found, clean up the lock");
                  case reset_lock (db) is
                     when True => null;
                     when False =>
                        Event.emit_debug (moderate, "unexpected failure of reset lock");
                        return False;
                  end case;
                  if upgrade then
                     --  In case of upgrade we should obtain a lock from the beginning,
                     --  hence switch upgrade to retain
                     case remove_lock_pid (db, Unix.getpid) is
                        when lock_okay =>
                           return obtain_lock (db, lock);
                        when lock_fatal | lock_end =>
                           Event.emit_debug (moderate, "unexpected failure of remove lock pid");
                           return False;
                     end case;
                  else
                     --  We might have inconsistent db, or some strange issue, so
                     --  just insert new record and go forward
                     case remove_lock_pid (db, Unix.getpid) is
                        when lock_okay =>
                           case CommonSQL.exec (db.handle, reset_state) is
                              when RESULT_OK =>
                                 return obtain_lock (db, lock);
                              when others =>
                                 --  reset_state was expected to be successul
                                 return False;
                           end case;
                        when lock_fatal | lock_end =>
                           Event.emit_debug (moderate, "unexpected failure of remove lock pid");
                           return False;
                     end case;
                  end if;
               when lock_fatal =>
                  Event.emit_debug (moderate, "unexpected failure of check pid, aborting.");
                  return False;
               when lock_okay =>
                  if timeout > 0 then
                     Event.emit_debug (moderate, "waiting for database lock for" & tries'Img &
                                         " times, next try in " & timeout'Img & " seconds");
                     delay Duration (timeout);
                  else
                     exit;
                  end if;
            end case;
         elsif not upgrade then
            locked_it := write_lock_pid (db);
            exit;
         else
            locked_it := True;
            exit;
         end if;
         tries := tries + 1;
      end loop;

      return locked_it;

   end try_lock;


   --------------------
   --  upgrade_lock  --
   --------------------
   function upgrade_lock
     (db       : in out RDB_Connection;
      old_type : lock_type;
      new_type : lock_type) return Boolean
   is
      sql : constant String := "UPDATE lock_state SET exclusive=1,advisory=1 " &
                               "WHERE exclusive=0 AND advisory=1 AND read=0;";
      upgraded : Boolean := False;
   begin
      if old_type = lock_advisory and new_type = lock_exclusive then
         Event.emit_debug(moderate, "want to upgrade advisory to exclusive lock");
         upgraded := try_lock (db, sql, new_type, true);
      end if;
      return upgraded;
   end upgrade_lock;


   ----------------------
   --  downgrade_lock  --
   ----------------------
   function downgrade_lock
     (db       : in out RDB_Connection;
      old_type : lock_type;
      new_type : lock_type) return Boolean
   is
      sql : constant String := "UPDATE lock_state SET exclusive=0,advisory=1 " &
                               "WHERE exclusive=1 AND advisory=1 AND read=0;";
      downgraded : Boolean := False;
   begin
      if old_type = lock_exclusive and new_type = lock_advisory then
         Event.emit_debug(moderate, "want to downgrade exclusive to advisory lock");
         downgraded := try_lock (db, sql, new_type, true);
      end if;
      return downgraded;
   end downgrade_lock;


   --------------------
   --  release_lock  --
   --------------------
   function release_lock
     (db       : in out RDB_Connection;
      lock     : lock_type) return Boolean
   is
      sql_read  : constant String := "UPDATE lock_state SET read=read-1 WHERE read>0;";
      sql_advi  : constant String := "UPDATE lock_state SET advisory=0 WHERE advisory=1;";
      sql_excl  : constant String := "UPDATE lock_state SET exclusive=0 WHERE exclusive=1;";
      func      : constant String := "release_lock";
      save      : constant String := "UNLOCK";
      read_lock : constant Boolean := RCU.config_setting (RCU.CFG.read_lock);

      procedure start_trax is
      begin
         if not CommonSQL.transaction_begin (db.handle, internal_srcfile, func, save) then
            Event.emit_error (func & ": Failed to start transaction");
         end if;
      end start_trax;

      procedure abort_trax is
      begin
         if not CommonSQL.transaction_rollback (db.handle, internal_srcfile, func, save) then
            Event.emit_error (func & ": Failed to rollback transaction");
         end if;
      end abort_trax;

      procedure complete_trax is
      begin
         if not CommonSQL.transaction_commit (db.handle, internal_srcfile, func, save) then
            Event.emit_error (func & ": Failed to commit transaction");
         end if;
      end complete_trax;
   begin
      if not SQLite.db_connected (db.handle) then
         return True;
      end if;
      case lock is
         when lock_readonly =>
            if not read_lock then
               return True;
            end if;
            Event.emit_debug(moderate, "release a read only lock on a database");
            start_trax;
            case CommonSQL.exec (db.handle, sql_read) is
               when RESULT_OK => null;
               when others =>
                  abort_trax;
                  return False;
            end case;
         when lock_advisory =>
            Event.emit_debug(moderate, "release an advisory lock on a database");
            start_trax;
            case CommonSQL.exec (db.handle, sql_advi) is
               when RESULT_OK => null;
               when others =>
                  abort_trax;
                  return False;
            end case;
         when lock_exclusive =>
            Event.emit_debug(moderate, "release an exclusive lock on a database");
            start_trax;
            case CommonSQL.exec (db.handle, sql_excl) is
               when RESULT_OK => null;
               when others =>
                  abort_trax;
                  return False;
            end case;
      end case;
      if SQLite.get_number_of_changes (db.handle) = 0 then
         complete_trax;
         return True;
      end if;

      case remove_lock_pid (db, Unix.getpid) is
         when lock_okay | lock_end =>
            complete_trax;
            return True;
         when lock_fatal =>
            abort_trax;
            return False;
      end case;
   end release_lock;


end Raven.Database.Lock;
