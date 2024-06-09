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
   function rdb_open_localdb (db : in out RDB_Connection;
                              contents : RDB_Contents) return Action_Result
   is
      func : constant String := "rdb_open_localdb()";
   begin
      case establish_localhost_connection (db, contents) is
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
   function establish_localhost_connection (db : in out RDB_Connection;
                                            contents : RDB_Contents) return Action_Result
   is
      func  : constant String := "establish_localhost_connection()";
      dbdir : constant String := RCU.config_setting (RCU.CFG.dbdir);
      key   : constant String := RCU.CFG.get_ci_key (RCU.CFG.dbdir);
      dirfd : Unix.File_Descriptor;
      attrs : Archive.Unix.File_Characteristics;
      okay  : Boolean;
      create_local_db : Boolean := False;

      procedure emit_absent_db is
      begin
         case contents is
            when installed_packages => Event.emit_no_local_db;
            when catalog            => Event.emit_no_remote_db;
         end case;
      end emit_absent_db;
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
                  emit_absent_db;
                  return RESULT_FATAL;
            end;
         when others =>
            Event.emit_error (func & ": " & key & " exists but is not a directory");
            emit_absent_db;
            return RESULT_FATAL;
      end case;

      dirfd := Context.reveal_db_directory_fd;
      if not Unix.file_connected (dirfd) then
         Event.emit_error (func & ": Failed to open " & key & " directory as a file descriptor");
         emit_absent_db;
         return RESULT_FATAL;
      end if;

      if not Unix.relative_file_readable (dirfd, database_filename (contents)) then
         if Unix.relative_file_exists (dirfd, database_filename (contents)) then
            --  db file exists but we can't read it, fail
            emit_absent_db;
            return RESULT_ENODB;
         elsif not Unix.relative_file_writable (dirfd, ".") then
            --  We need to create db file but we can't write to the containing
            --  directory, so fail
            emit_absent_db;
            return RESULT_ENODB;
         else
            create_local_db := True;
         end if;
      end if;

      okay := SQLite.initialize_sqlite;
      SQLite.rdb_syscall_overload;

      if not SQLite.open_sqlite_database_readwrite
        ("/" & database_filename (contents), db.handle'Access)
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
         emit_absent_db;
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
            emit_absent_db;
            return RESULT_FATAL;
         end if;
      end if;

       --  Create custom functions
      CustomCmds.define_three_functions (db.handle);

      if not upgrade_schema (db) then
         emit_absent_db;
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
            emit_absent_db;
            return RESULT_FATAL;
         end if;
      end;

      declare
         sql : constant String := "PRAGMA mmap_size=268435456";
      begin
         if not SQLite.exec_sql (db.handle, sql) then
            CommonSQL.ERROR_SQLITE (db.handle, internal_srcfile, func, sql);
            rdb_close (db);
            emit_absent_db;
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
                  null;
               end if;
               return False;
         end case;
      end loop;

      if not CommonSQL.transaction_commit (db.handle, internal_srcfile,func, "") then
         Event.emit_error (func & ": failed transaction commit");
         if not CommonSQL.transaction_rollback (db.handle, internal_srcfile, func, save) then
            null;
         end if;
         return False;
      end if;
      return True;
   end create_localhost_database;


   --------------------------------
   --  create_rvnindex_database  --
   --------------------------------
   function create_rvnindex_database (db : in out RDB_Connection) return Boolean
   is
      func : constant String := "create_rvnindex_database";
      sqlt : constant String := "CREATE TABLE rvnindex (" &
        "subpackage_id INTEGER PRIMARY KEY, " &
        "subpackage_name TEXT NOT NULL, " &
        "port_version TEXT NOT NULL)";
      sqli : constant String := "CREATE UNIQUE INDEX nsvv ON rvnindex " &
        "(subpackage_name, port_version)";
   begin
      case CommonSQL.exec (db.handle, sqlt) is
            when RESULT_OK => null;
            when others =>
               Event.emit_error (func & ": failed to create rvnindex table");
               return False;
      end case;
      case CommonSQL.exec (db.handle, sqli) is
         when RESULT_OK => null;
         when others =>
            Event.emit_error (func & ": failed to create index");
            return False;
      end case;
      return True;
   end create_rvnindex_database;


   --------------------------------
   --  establish_rvn_connection  --
   --------------------------------
   function establish_rvn_connection
     (db           : in out RDB_Connection;
      truncate_db  : Boolean;
      index_dbdir  : String;
      index_dbname : String) return Action_Result
   is
      func  : constant String := "establish_rvn_connection()";
      dirfd : Unix.File_Descriptor;
      attrs : Archive.Unix.File_Characteristics;
      okay  : Boolean;
      create_local_db : Boolean := False;
      dirflags : constant Unix.T_Open_Flags := (DIRECTORY => True,
                                                CLOEXEC => True,
                                                others => False);
   begin
      if SQLite.db_connected (db.handle) then
         return RESULT_OK;
      end if;

      Event.emit_debug (moderate, internal_srcfile & ": " & func);

      attrs := UNX.get_charactistics (index_dbdir);
      case attrs.ftype is
         when Archive.directory => null;
         when Archive.unsupported =>
            begin
               DIR.Create_Path (index_dbdir);
            exception
               when others =>
                  Event.emit_error (func & ": Failed to create rvnindex database directory");
                  Event.emit_no_local_db;
                  return RESULT_FATAL;
            end;
         when others =>
            Event.emit_error (func & ": " & index_dbdir & " exists but is not a directory");
            Event.emit_no_local_db;
            return RESULT_FATAL;
      end case;

      dirfd := Unix.open_file (index_dbdir, dirflags);
      if not Unix.file_connected (dirfd) then
         Event.emit_error (func & ": Failed to open database directory as a file descriptor");
         Event.emit_no_local_db;
         return RESULT_FATAL;
      end if;

      if Unix.relative_file_readable (dirfd, index_dbname) then
         if truncate_db then
            begin
               DIR.Delete_File (index_dbdir & "/" & index_dbname);
               create_local_db := True;
            exception
               when others =>
                  Unix.close_file_blind (dirfd);
                  Event.emit_error ("Failed to delete existing database: " & index_dbname);
                  Event.emit_no_local_db;
                  return RESULT_ENODB;
            end;
         end if;
      else
         if Unix.relative_file_exists (dirfd, index_dbname) then
            --  db file exists but we can't read it.
            if truncate_db then
               begin
                  DIR.Delete_File (index_dbdir & "/" & index_dbname);
                  create_local_db := True;
               exception
                  when others =>
                     --  Could not delete database file, bail out
                     Unix.close_file_blind (dirfd);
                     Event.emit_no_local_db;
                     return RESULT_ENODB;
               end;
            else
               Unix.close_file_blind (dirfd);
               Event.emit_no_local_db;
               return RESULT_ENODB;
            end if;
         elsif not Unix.relative_file_writable (dirfd, ".") then
            --  We need to create db file but we can't write to the containing
            --  directory, so fail
            Unix.close_file_blind (dirfd);
            Event.emit_no_local_db;
            return RESULT_ENODB;
         else
            create_local_db := True;
         end if;
      end if;
      Unix.close_file_blind (dirfd);

      okay := SQLite.initialize_sqlite;
      SQLite.rdb_syscall_overload;

      if not SQLite.open_sqlite_database_readwrite ("/" & index_dbname, db.handle'Access)
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
         Event.emit_no_local_db;
         return RESULT_FATAL;
      end if;

      --  The database file is blank when create is set, so we have to initialize it
      if create_local_db then
         Event.emit_debug (moderate, func & ": import initial schema to blank rvnindex db");
         if not create_rvnindex_database (db) then
            rdb_close (db);
            Event.emit_no_local_db;
            return RESULT_FATAL;
         end if;
      end if;

      return RESULT_OK;

   end establish_rvn_connection;


   --------------------------------------
   --  initialize_prepared_statements  --
   --------------------------------------
   function initialize_prepared_statements (db : in out RDB_Connection) return Boolean
   is
      func : constant String := "initialize_prepared_statements()";
   begin
      for stmt in SCH.prepared_statement loop
         declare
            sql : constant String := SCH.prstat_definition (stmt);
            key : constant String :=  pad_right (stmt'Img, 12);
         begin
            Event.emit_debug (low_level, "rdb: init " & key & " > " & SQ (sql));
            if not SQLite.prepare_sql (db.handle, sql, prepared_statements (stmt)) then
               CommonSQL.ERROR_SQLITE (db.handle, internal_srcfile, func,
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
      db.prstmt_initialized := False;
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
         if not Archive.Unix.user_is_root then
            Event.emit_error ("Switching to superuser privileges might resolve this.");
         end if;
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

            Event.emit_debug (high_level, "executing " & SQ (sql));
            if not SQLite.exec_sql (db.handle, sql) then
               Event.emit_error ("schema update failed");
               if not CommonSQL.transaction_rollback (db.handle, internal_srcfile, func, "") then
                  Event.emit_error ("schema update rollback failed.");
               end if;
               return False;
            end if;

            Event.emit_debug (high_level, "executing " & SQ (pragsql));
            if not SQLite.exec_sql (db.handle, pragsql) then
               Event.emit_error ("schema update failed");
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
   function localdb_exists (contents : RDB_Contents) return Boolean
   is
      dirfd : Unix.File_Descriptor;
   begin
      dirfd := Context.reveal_db_directory_fd;
      return Unix.relative_file_exists (dirfd, database_filename (contents));
   end localdb_exists;


   --------------------
   --  localdb_path  --
   --------------------
   function localdb_path (contents : RDB_Contents) return String
   is
      dbdir : constant String := RCU.config_setting (RCU.CFG.dbdir);
   begin
      return dbdir & "/" & database_filename (contents);
   end localdb_path;


   --------------------------------------
   --  initialize_rvnindex_statements  --
   --------------------------------------
   function initialize_rvnindex_statements (db : in out RDB_Connection) return Boolean
   is
      sql : constant String := "INSERT INTO rvnindex (subpackage_name, port_version) VALUES (?,?)";
      func : constant String := "initialize_rvnindex_statements()";
   begin
      Event.emit_debug (low_level, "rvnindex db: init insertion prepared statement");
      if not SQLite.prepare_sql (db.handle, sql, rvnindex_statement) then
         CommonSQL.ERROR_SQLITE (db.handle, internal_srcfile, func, sql);
         return False;
      end if;
      return True;
   end initialize_rvnindex_statements;


   ------------------------------------
   --  finalize_rvnindex_statements  --
   ------------------------------------
   procedure finalize_rvnindex_statements (db : in out RDB_Connection) is
   begin
      if db.prstmt_initialized then
         Event.emit_debug (low_level, "rvnindex db: close prepared statement");
         SQLite.finalize_statement (rvnindex_statement);
      end if;
      db.prstmt_initialized := False;
   end finalize_rvnindex_statements;


   ----------------------
   --  rindex_db_open  --
   ----------------------
   function rindex_db_open
     (db           : in out RDB_Connection;
      truncate_db  : Boolean;
      index_dbdir  : String;
      index_dbname : String)  return Action_Result
   is
      func : constant String := "rindex_db_open()";
   begin
      case establish_rvn_connection (db, truncate_db, index_dbdir, index_dbname) is
         when RESULT_OK =>
            if not db.prstmt_initialized then
               if not initialize_rvnindex_statements (db) then
                  Event.emit_error (func & ": Failed to initialize prepared statements");
                  rindex_db_close (db);
                  return RESULT_FATAL;
               end if;
               db.prstmt_initialized := True;
            end if;
            return RESULT_OK;
         when others =>
            return RESULT_FATAL;
      end case;
   end rindex_db_open;


   -----------------------
   --  rindex_db_close  --
   -----------------------
   procedure rindex_db_close (db : in out RDB_Connection)
   is
      use type SQLite.db3;
   begin
      finalize_rvnindex_statements (db);
      if db.handle /= null then
         SQLite.close_database (db.handle);
         db.handle := null;
      end if;
      SQLite.shutdown_sqlite;
   end rindex_db_close;

end Raven.Database.Operations;
