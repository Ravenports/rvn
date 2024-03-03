--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Text_IO;
with Ada.Strings.Fixed;
with Raven.Database.Operations;
with Raven.Database.CommonSQL;
with Raven.Event;
with Raven.Strings;

use Raven.Strings;


package body Raven.Database.Cmdversion is

   package TIO renames Ada.Text_IO;
   package ASF renames Ada.Strings.Fixed;
   package CSQ renames Raven.Database.CommonSQL;
   package DOP renames Raven.Database.Operations;

   -----------------------
   --  create_rvnindex  --
   -----------------------
   function create_rvnindex
     (rvndb              : in out Database.RDB_Connection;
      database_directory : String;
      database_file_path : String;
      rvnindex_file_path : String) return Boolean
   is
      savepoint : constant String := "POPULATE";
      srcfile   : constant String := "raven-cmd-version.adb";
      func      : constant String := "create_index_database";
      rb_msg    : constant String := "Failed to rollback transaction on " & database_file_path;
      handle    : TIO.File_Type;
      revert    : Boolean := False;
   begin
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
         return False;
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
         return False;
      end if;

      if not CSQ.transaction_commit (rvndb.handle, srcfile, func, savepoint) then
         Event.emit_error ("Failed to commit transaction on " & database_file_path);
         if CSQ.transaction_rollback (rvndb.handle, srcfile, func, savepoint) then
            Event.emit_error (rb_msg);
         end if;
         return False;
      end if;
      DOP.rindex_db_close (rvndb);
      return True;

   end create_rvnindex;

end Raven.Database.Cmdversion;
