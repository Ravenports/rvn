--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Directories;
with Raven.Database.CommonSQL;
with Raven.Database.CustomCmds;
with Raven.Strings;
with Raven.Context;
with Raven.Cmd.Unset;
with Raven.Event;
with Raven.Unix;
with Archive.Unix;
with SQLite;

use Raven.Strings;

package body Raven.Database.Operations is

   package DIR renames Ada.Directories;
   package UNX renames Archive.Unix;
   package RCU renames Raven.Cmd.Unset;
   package SCH renames Raven.Database.Schema;


   ------------------------
   --  rdb_open_localdb  --
   ------------------------
   function rdb_open_localdb (db : in out RDB_Connection) return Action_Result
   is
      func : constant String := "rdb_open_localdb()";
   begin
      case establish_localhost_connection (db) is
         when RESULT_OK =>
            if not db.prstmt_initialized then
               if not initialize_prepared_statements (db) then
                  Event.emit_error (func & ": Failed to initialize prepared statements");
                  rdb_close (db);
                  return RESULT_FATAL;
               end if;
               db.prstmt_initialized := True;
            end if;
            return RESULT_OK;
         when others =>
            return RESULT_FATAL;
      end case;
   end rdb_open_localdb;


   -----------------
   --  rdb_close  --
   -----------------
   procedure rdb_close (db : in out RDB_Connection)
   is
      use type SQLite.db3;
   begin
      finalize_prepared_statements (db);
      if db.handle /= null then
         SQLite.close_database (db.handle);
         db.handle := null;
      end if;
      SQLite.shutdown_sqlite;
   end rdb_close;


   --------------------------------------
   --  establish_localhost_connection  --
   --------------------------------------
   function establish_localhost_connection (db : in out RDB_Connection) return Action_Result
   is
      func  : constant String := "establish_localhost_connection()";
      dbdir : constant String := RCU.config_setting (RCU.CFG.dbdir);
      key   : constant String := RCU.CFG.get_ci_key (RCU.CFG.dbdir);
      dirfd : Unix.File_Descriptor;
      attrs : Archive.Unix.File_Characteristics;
      okay  : Boolean;
      create_local_db : Boolean := False;
   begin
      if SQLite.db_connected (db.handle) then
         return RESULT_OK;
      end if;

      Event.emit_debug (moderate, internal_srcfile & ": " & func);

      attrs := UNX.get_charactistics (dbdir);
      case attrs.ftype is
         when Archive.directory => null;
         when Archive.unsupported =>
            begin
               DIR.Create_Path (dbdir);
            exception
               when others =>
                  Event.emit_error (func & ": Failed to create " & key & " directory");
                  return RESULT_FATAL;
            end;
         when others =>
            Event.emit_error (func & ": " & key & " exists but is not a directory");
            return RESULT_FATAL;
      end case;

      dirfd := Context.reveal_db_directory_fd;
      if not Unix.file_connected (dirfd) then
         Event.emit_error (func & ": Failed to open " & key & " directory as a file descriptor");
         return RESULT_FATAL;
      end if;

      if not Unix.relative_file_readable (dirfd, localhost_database) then
         if Unix.relative_file_exists (dirfd, localhost_database) then
            --  db file exists but we can't read it, fail
            Event.emit_no_local_db;
            return RESULT_ENODB;
         elsif not Unix.relative_file_writable (dirfd, ".") then
            --  We need to create db file but we can't write to the containing
            --  directory, so fail
            Event.emit_no_local_db;
            return RESULT_ENODB;
         else
            create_local_db := True;
         end if;
      end if;

      okay := SQLite.initialize_sqlite;
      SQLite.rdb_syscall_overload;

      if not SQLite.open_sqlite_database_readwrite ("/" & localhost_database, db.handle'Access)
      then
         CommonSQL.ERROR_SQLITE (db      => db.handle,
                                 srcfile => internal_srcfile,
                                 func    => func,
                                 query   => "sqlite open");
         if SQLite.database_corrupt (db.handle) then
            Event.emit_error
              (func & ": Database corrupt.  Are you running on NFS?  " &
                 "If so, ensure the locking mechanism is properly set up.");
         end if;
         return RESULT_FATAL;
      end if;

      --  Wait up to 5 seconds if database is busy
      if not SQLite.set_busy_timeout (db.handle, 5000) then
         Event.emit_error (func & ": Failed to set busy timeout");
      end if;

      --  The database file is blank when create is set, so we have to initialize it
      if create_local_db then
         Event.emit_debug (moderate, func & ": import initial schema to blank rvn db");
         if not create_localhost_database (db) then
            rdb_close (db);
            return RESULT_FATAL;
         end if;
      end if;

       --  Create custom functions
      CustomCmds.define_three_functions (db.handle);

      if not upgrade_schema (db) then
         return RESULT_FATAL;
      end if;

      --  allow foreign key option which will allow to have
      --  clean support for reinstalling
      declare
         sql : constant String := "PRAGMA foreign_keys = ON";
      begin
         if not SQLite.exec_sql (db.handle, sql) then
            CommonSQL.ERROR_SQLITE (db.handle, internal_srcfile, func, sql);
            rdb_close (db);
            return RESULT_FATAL;
         end if;
      end;

      declare
         sql : constant String := "PRAGMA mmap_size=268435456";
      begin
         if not SQLite.exec_sql (db.handle, sql) then
            CommonSQL.ERROR_SQLITE (db.handle, internal_srcfile, func, sql);
            rdb_close (db);
            return RESULT_FATAL;
         end if;
      end;

      return RESULT_OK;

   end establish_localhost_connection;


   ---------------------------------
   --  create_localhost_database  --
   ---------------------------------
   function create_localhost_database (db : in out RDB_Connection) return Boolean
   is
      func : constant String := "create_localhost_database";
      save : constant String := "GENESIS";
   begin
      if not CommonSQL.transaction_begin (db.handle, internal_srcfile, func, save) then
         Event.emit_error (func & ": Failed to start transaction");
         return False;
      end if;

      for comp in SCH.schema_component'Range loop
         case CommonSQL.exec (db.handle, SCH.component_definition (comp)) is
            when RESULT_OK => null;
            when others =>
               Event.emit_error (func & ": failed component " & comp'Img);
               if not CommonSQL.transaction_rollback (db.handle, internal_srcfile, func, save) then
                  return False;
               end if;
         end case;
      end loop;
      if not CommonSQL.transaction_commit (db.handle, internal_srcfile,func, "") then
         Event.emit_error (func & ": failed transaction commit");
         if not CommonSQL.transaction_rollback (db.handle, internal_srcfile, func, save) then
            return False;
         end if;
      end if;
      return True;
   end create_localhost_database;


   --------------------------------------
   --  initialize_prepared_statements  --
   --------------------------------------
   function initialize_prepared_statements (db : in out RDB_Connection) return Boolean
   is
   begin
      for stmt in SCH.prepared_statement loop
         declare
            sql : constant String := SCH.prstat_definition (stmt);
            key : constant String :=  pad_right (stmt'Img, 12);
         begin
            Event.emit_debug (low_level, "rdb: init " & key & " > " & SQ (sql));
            if not SQLite.prepare_sql (db.handle, sql, prepared_statements (stmt)) then
               CommonSQL.ERROR_SQLITE (db.handle, internal_srcfile, "repo_prstmt_initialize",
                                       SCH.prstat_definition (stmt));
               return False;
            end if;
         end;
      end loop;
      return True;
   end initialize_prepared_statements;


   ------------------------------------
   --  finalize_prepared_statements  --
   ------------------------------------
   procedure finalize_prepared_statements (db : in out RDB_Connection) is
   begin
      if db.prstmt_initialized then
         for stmt in SCH.prepared_statement loop
            declare
               key : constant String :=  pad_right (stmt'Img, 12);
            begin
               Event.emit_debug (low_level, "rdb: close " & key & " statement");
               SQLite.finalize_statement (prepared_statements (stmt));
            end;
         end loop;
      end if;
   end finalize_prepared_statements;


   ----------------------
   --  upgrade_schema  --
   ----------------------
   function upgrade_schema (db : in out RDB_Connection) return Boolean
   is
      func      : constant String := "rdb_upgrade";
      exp_dbver : constant int64 := int64 (LOCAL_SCHEMA_VERSION);
      cur_dbver : int64;
   begin
      if not CommonSQL.get_int64 (db      => db.handle,
                                  srcfile => internal_srcfile,
                                  func    => func,
                                  sql     => "PRAGMA user_version",
                                  res     => cur_dbver,
                                  silence => False)
      then
         return false;
      end if;

      if cur_dbver = exp_dbver then
         return True;
      end if;

      if cur_dbver < 0 then
         Event.emit_error ("FATAL: database version " & cur_dbver'Img & " is negative");
         return False;
      end if;

      --  Rare case where database version on file is greater than maximum known version
      --  This should only happen with developers

      if cur_dbver > exp_dbver then
         Event.emit_notice
           ("warning: database version" & cur_dbver'Img & " is newer than the latest "
            & progname & "version" & exp_dbver'Img);
         return True;
      end if;

      if SQLite.database_was_opened_readonly (db.handle, SQLite.primary_db_identity) then
         Event.emit_error ("The database is outdated and opened readonly");
         return False;
      end if;

      for step in Natural (cur_dbver + 1) .. LOCAL_SCHEMA_VERSION loop
         declare
            sql : constant String := SCH.upgrade_definition (Local_Upgrade_Series (step));
            pragsql : constant String := "PRAGMA user_version =" & step'Img & ";";
         begin
            if not CommonSQL.transaction_begin (db.handle, internal_srcfile, func, "") then
               Event.emit_error ("schema update transaction begin failed");
               return False;
            end if;

            if not SQLite.exec_sql (db.handle, sql) then
               Event.emit_error ("schema update failed, sql: " & sql);
               if not CommonSQL.transaction_rollback (db.handle, internal_srcfile, func, "") then
                  Event.emit_error ("schema update rollback failed.");
               end if;
               return False;
            end if;

            if not SQLite.exec_sql (db.handle, pragsql) then
               Event.emit_error ("schema update failed, sql: " & pragsql);
               if not CommonSQL.transaction_rollback (db.handle, internal_srcfile, func, "") then
                  Event.emit_error ("schema update rollback failed.");
               end if;
               return False;
            end if;

            if not CommonSQL.transaction_commit (db.handle, internal_srcfile, func, "") then
               Event.emit_error ("schema update transaction commit failed");
               return False;
            end if;
         end;
      end loop;
      return True;
   end upgrade_schema;


   ----------------------
   --  localdb_exists  --
   ----------------------
   function localdb_exists return Boolean
   is
      dirfd : Unix.File_Descriptor;
   begin
      dirfd := Context.reveal_db_directory_fd;
      return Unix.relative_file_exists (dirfd, localhost_database);
   end localdb_exists;


   --------------------
   --  localdb_path  --
   --------------------
   function localdb_path return String
   is
      dbdir : constant String := RCU.config_setting (RCU.CFG.dbdir);
   begin
      return dbdir & "/" & localhost_database;
   end localdb_path;


end Raven.Database.Operations;
