--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Directories;
with Raven.Database.CommonSQL;
with Raven.Database.Schema;
with Raven.Database.CustomCmds;
with Raven.Context;
with Raven.Cmd.Unset;
with Raven.Event;
with Raven.Unix;
with Archive.Unix;
with SQLite;

package body Raven.Database.Operations is

   package DIR renames Ada.Directories;
   package UNX renames Archive.Unix;
   package RCU renames Raven.Cmd.Unset;
   package SCH renames Raven.Database.Schema;

   -----------------
   --  rdb_close  --
   -----------------
   procedure rdb_close (db : in out RDB_Connection)
   is
      use type SQLite.db3;
   begin
      if db.prstmt_initialized then
         --  TODO Schema.local_prstmt_finalize (db);
         null;
      end if;
      if db.handle /= null then
         --  TODO ROP.close_all_open_repositories;
         SQLite.close_database (db.handle);
         db.handle := null;
      end if;
      SQLite.shutdown_sqlite;
   end rdb_close;


   ----------------------------
   --  establish_connection  --
   ----------------------------
   function establish_connection (db : in out RDB_Connection) return Action_Result
   is
      func  : constant String := "establish_connection";
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
            rdb_close (db);
            return RESULT_ENODB;
         elsif not Unix.relative_file_writable (dirfd, ".") then
            --  We need to create db file but we can't write to the containing
            --  directory, so fail
            Event.emit_no_local_db;
            rdb_close (db);
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
         rdb_close (db);
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
      CustomCmds.define_six_functions (db.handle);

      --  TODO rdb_upgrade

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

      return RESULT_OK;

   end establish_connection;


   ---------------------------------
   --  create_localhost_database  --
   ---------------------------------
   function create_localhost_database (db : in out RDB_Connection) return Boolean
   is
      func : constant String := "create_localhost_database";
   begin
      if not CommonSQL.transaction_begin (db.handle, internal_srcfile, func, "") then
         Event.emit_error (func & ": Failed to start transaction");
         return False;
      end if;

      for comp in SCH.schema_component'Range loop
         case CommonSQL.exec (db.handle, SCH.component_definition (comp)) is
            when RESULT_OK => null;
            when others =>
               Event.emit_error (func & ": failed component " & comp'Img);
               if not CommonSQL.transaction_rollback (db.handle, internal_srcfile, func, "") then
                  return False;
               end if;
         end case;
      end loop;
      if not CommonSQL.transaction_commit (db.handle, internal_srcfile,func, "") then
         Event.emit_error (func & ": failed transaction commit");
         if not CommonSQL.transaction_rollback (db.handle, internal_srcfile, func, "") then
            return False;
         end if;
      end if;
      return True;
   end create_localhost_database;


end Raven.Database.Operations;
