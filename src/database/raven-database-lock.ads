--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

private with Raven.Unix;

package Raven.Database.Lock is

   type lock_type is (lock_readonly, lock_advisory, lock_exclusive);

   function reset_lock (db : in out RDB_Connection) return Boolean;

   function obtain_lock
     (db       : in out RDB_Connection;
      lock     : lock_type) return Boolean;

   function release_lock
     (db       : in out RDB_Connection;
      lock     : lock_type) return Boolean;

   function upgrade_lock
     (db       : in out RDB_Connection;
      old_type : lock_type;
      new_type : lock_type) return Boolean;

   function downgrade_lock
     (db       : in out RDB_Connection;
      old_type : lock_type;
      new_type : lock_type) return Boolean;

   no_read_lock : constant String := "Cannot get a read lock on a database (already locked)";
   no_adv_lock : constant String := "Cannot get a advisory lock on a database (already locked)";
   no_exc_lock : constant String := "Cannot get a exclusive lock on a database (already locked)";

   no_read_unlock      : constant String := "Failed to release database read lock";
   no_advisory_unlock  : constant String := "Failed to release database advisory lock";
   no_exclusive_unlock : constant String := "Failed to release database exclusive lock";

private

   internal_srcfile : constant String := "raven-database-locks.adb";

   type lock_result is (lock_okay, lock_end, lock_fatal);

   function check_lock_pid (db : in out RDB_Connection) return lock_result;
   function write_lock_pid (db : in out RDB_Connection) return boolean;

   function remove_lock_pid
     (db  : in out RDB_Connection;
      pid : Unix.Process_ID) return lock_result;

   function try_lock
     (db       : in out RDB_Connection;
      lock_sql : String;
      lock     : lock_type;
      upgrade  : Boolean) return Boolean;

end Raven.Database.Lock;
