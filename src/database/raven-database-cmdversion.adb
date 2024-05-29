--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Text_IO;
with Ada.Strings.Fixed;
with Raven.Database.Operations;
with Raven.Database.CommonSQL;
with Raven.Event;
with Raven.Strings;
with Raven.Pkgtypes;

use Raven.Strings;
use Raven.Pkgtypes;

package body Raven.Database.Cmdversion is

   package TIO renames Ada.Text_IO;
   package ASF renames Ada.Strings.Fixed;
   package CSQ renames Raven.Database.CommonSQL;
   package DOP renames Raven.Database.Operations;

   -----------------------
   --  create_rvnindex  --
   -----------------------
   function create_rvnindex
     (database_directory : String;
      database_file_path : String;
      rvnindex_file_path : String) return Boolean
   is
      savepoint : constant String := "POPULATE";
      srcfile   : constant String := "raven-cmd-version.adb";
      func      : constant String := "create_index_database";
      rb_msg    : constant String := "Failed to rollback transaction on " & database_file_path;
      rvndb     : Database.RDB_Connection;
      handle    : TIO.File_Type;
      revert    : Boolean := False;
   begin
      Event.emit_debug (moderate, "Building fresh rvnindex database.");
      case Operations.rindex_db_open
        (db           => rvndb,
         truncate_db  => True,
         index_dbdir  => database_directory,
         index_dbname => tail (database_file_path, "/")) is
         when RESULT_OK =>
            null;
         when others =>
            return False;
      end case;

      if not CSQ.transaction_begin (rvndb.handle, srcfile, func, savepoint) then
         Event.emit_error ("Failed to start transaction on " & database_file_path);
         goto cleanup_mess;
      end if;

      begin
         TIO.Open (handle, TIO.In_File, rvnindex_file_path);
         while not TIO.End_Of_File (handle) loop
            declare
               line : constant String := ASF.Trim (TIO.Get_Line (handle), Ada.Strings.Both);
            begin
               if count_char (line, ' ') > 0 then
                  if not SQLite.reset_statement (DOP.rvnindex_statement) then
                     Event.emit_error ("Failed to reset rvnindex prepared stmt");
                     revert := True;
                     exit;
                  end if;
                  SQLite.bind_string (DOP.rvnindex_statement, 1, specific_field (line, 1, " "));
                  SQLite.bind_string (DOP.rvnindex_statement, 2, specific_field (line, 2, " "));
                  Event.emit_debug (high_level, "[rvnindex] running: "
                                    & SQLite.get_expanded_sql (DOP.rvnindex_statement));
                  case SQLite.step (DOP.rvnindex_statement) is
                     when SQLite.no_more_data => null;
                     when SQLite.row_present | SQLite.something_else =>
                        CommonSQL.ERROR_STMT_SQLITE
                          (rvndb.handle, internal_srcfile, func,
                           SQLite.get_expanded_sql (DOP.rvnindex_statement));
                        revert := True;
                        exit;
                  end case;
               end if;
            end;
         end loop;
      exception
         when others =>
            Event.emit_error ("Failed to open " & rvnindex_file_path);
            revert := True;
      end;
      if TIO.Is_Open (handle) then
         TIO.Close (handle);
      end if;
      if revert then
         if CSQ.transaction_rollback (rvndb.handle, srcfile, func, savepoint) then
            Event.emit_error (rb_msg);
         end if;
         goto cleanup_mess;
      end if;

      if not CSQ.transaction_commit (rvndb.handle, srcfile, func, savepoint) then
         Event.emit_error ("Failed to commit transaction on " & database_file_path);
         if CSQ.transaction_rollback (rvndb.handle, srcfile, func, savepoint) then
            Event.emit_error (rb_msg);
         end if;
         goto cleanup_mess;
      end if;
      DOP.rindex_db_close (rvndb);
      return True;

      <<cleanup_mess>>
      DOP.rindex_db_close (rvndb);
      return False;

   end create_rvnindex;


   -----------------------------------
   --  map_nsv_to_rvnindex_version  --
   -----------------------------------
   procedure map_nsv_to_rvnindex_version
     (database_directory : String;
      database_file_path : String;
      version_map : in out NV_Pairs.Map)
   is
      func : constant String := "map_nsv_to_rvnindex_version";
      sql  : constant String := "SELECT subpackage_name, version from rvnindex";
      rvndb    : Database.RDB_Connection;
      new_stmt : SQLite.thick_stmt;
   begin
      version_map.clear;
      case Operations.rindex_db_open
        (db           => rvndb,
         truncate_db  => False,
         index_dbdir  => database_directory,
         index_dbname => tail (database_file_path, "/")) is
         when RESULT_OK => null;
         when others => return;
      end case;

      if not SQLite.prepare_sql (rvndb.handle, sql, new_stmt) then
         CommonSQL.ERROR_STMT_SQLITE (rvndb.handle, internal_srcfile, func, sql);
         DOP.rindex_db_close (rvndb);
         return;
      end if;
      debug_running_stmt (new_stmt);
      loop
         case SQLite.step (new_stmt) is
            when SQLite.row_present =>
               declare
                  nsv : constant String := SQLite.retrieve_string (new_stmt, 0);
                  ver : constant String := SQLite.retrieve_string (new_stmt, 1);
               begin
                  version_map.Insert (SUS (nsv), SUS (ver));
               end;
            when SQLite.something_else =>
               CommonSQL.ERROR_STMT_SQLITE (rvndb.handle, internal_srcfile, func,
                                            SQLite.get_expanded_sql (new_stmt));
               exit;
            when SQLite.no_more_data => exit;
         end case;
      end loop;
      SQLite.finalize_statement (new_stmt);
      DOP.rindex_db_close (rvndb);
   end map_nsv_to_rvnindex_version;


   --------------------------------
   --  map_nsv_to_local_version  --
   --------------------------------
   procedure map_nsv_to_local_version
     (db           : RDB_Connection;
      behave_cs    : Boolean;
      behave_exact : Boolean;
      pattern      : String;
      version_map  : in out NV_Pairs.Map)
   is
      func : constant String := "map_nsv_to_local_version";
      bsql : constant String :=
        "SELECT namebase ||'-'|| subpackage ||'-'|| variant as nsv, version from packages";
      new_stmt : SQLite.thick_stmt;

      function sql return String is
      begin
         if IsBlank (pattern) then
            return bsql;
         end if;
         if behave_exact then
            return bsql & " WHERE nsv = ?";
         elsif behave_cs then
            return bsql &" WHERE nsv GLOB ?";
         end if;
         return bsql & " WHERE nsv LIKE ?";
      end sql;
   begin
      if not SQLite.prepare_sql (db.handle, sql, new_stmt) then
         CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func, sql);
         return;
      end if;
      if not IsBlank (pattern) then
         if not behave_exact and then not behave_cs then
            SQLite.bind_string (new_stmt, 1, pattern & '%');
         else
            SQLite.bind_string (new_stmt, 1, pattern);
         end if;
      end if;
      debug_running_stmt (new_stmt);
      loop
         case SQLite.step (new_stmt) is
            when SQLite.row_present =>
               declare
                  nsv : constant String := SQLite.retrieve_string (new_stmt, 0);
                  ver : constant String := SQLite.retrieve_string (new_stmt, 1);
               begin
                  version_map.Insert (SUS (nsv), SUS (ver));
               end;
            when SQLite.something_else =>
               CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                            SQLite.get_expanded_sql (new_stmt));
               exit;
            when SQLite.no_more_data => exit;
         end case;
      end loop;
      SQLite.finalize_statement (new_stmt);
   end map_nsv_to_local_version;

end Raven.Database.Cmdversion;
