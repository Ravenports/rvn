--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Containers.Vectors;
with Raven.Event;
with Raven.Strings;
with Raven.Database.CommonSQL;

use Raven.Strings;

package body Raven.Database.Triggers is

   ------------------------------
   --  get_directory_triggers  --
   ------------------------------
   procedure get_directory_triggers
     (db       : Database.RDB_Connection;
      dir_list : in out Directory_Triggers.Vector)
   is
      new_stmt : SQLite.thick_stmt;
      func     : constant String := "get_directory_triggers";
      sql      : constant String :=
        "SELECT p.namebase, p.subpackage, p.variant, p.prefix, t.code, path.path_value, " &
               "t.trigger_id, p.id " &
        "FROM pkg_triggers t " &
        "JOIN trigger_paths path on path.trigger_id = t.trigger_id " &
        "JOIN packages p on p.id = t.package_id " &
        "WHERE t.trigger_type = 1 AND path.path_type = 0";
   begin
      dir_list.Clear;
      if not SQLite.prepare_sql (db.handle, sql, new_stmt) then
         CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func, sql);
         return;
      end if;
      debug_running_stmt (new_stmt);

      loop
         case SQLite.step (new_stmt) is
            when SQLite.row_present =>
               declare
                  myrec : A_Directory_Trigger;
               begin
                  myrec.namebase := SUS (SQLite.retrieve_string (new_stmt, 0));
                  myrec.subpkg   := SUS (SQLite.retrieve_string (new_stmt, 1));
                  myrec.variant  := SUS (SQLite.retrieve_string (new_stmt, 2));
                  myrec.prefix   := SUS (SQLite.retrieve_string (new_stmt, 3));
                  myrec.script   := SUS (SQLite.retrieve_string (new_stmt, 4));
                  myrec.ent_path := SUS (SQLite.retrieve_string (new_stmt, 5));
                  myrec.trig_id  := Natural (SQLite.retrieve_integer (new_stmt, 6));
                  myrec.pkg_id   := Pkgtypes.Package_ID (SQLite.retrieve_integer (new_stmt, 7));
                  dir_list.Append (myrec);
               end;
            when SQLite.something_else =>
               CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                            SQLite.get_expanded_sql (new_stmt));
            when SQLite.no_more_data => exit;
         end case;
      end loop;
      SQLite.finalize_statement (new_stmt);
   end get_directory_triggers;


   -----------------------------
   --  file_triggers_present  --
   -----------------------------
   procedure file_triggers_present
     (db            : Database.RDB_Connection;
      pattern_exact : out Boolean;
      pattern_glob  : out Boolean;
      pattern_regex : out Boolean)
   is
      new_stmt : SQLite.thick_stmt;
      func     : constant String := "file_triggers_present";
      sql : constant String := "SELECT " &
        "(SELECT count(*) FROM trigger_paths WHERE path_type = 1), " &
        "(SELECT count(*) FROM trigger_paths WHERE path_type = 2), " &
        "(SELECT count(*) FROM trigger_paths WHERE path_type = 3)";

      use type SQLite.sql_int64;
   begin
      pattern_exact := False;
      pattern_glob  := False;
      pattern_regex := False;
      if not SQLite.prepare_sql (db.handle, sql, new_stmt) then
         CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func, sql);
         return;
      end if;
      debug_running_stmt (new_stmt);

       loop
         case SQLite.step (new_stmt) is
            when SQLite.row_present =>
               if SQLite.retrieve_integer (new_stmt, 0) > 0 then
                  pattern_exact := True;
               end if;
               if SQLite.retrieve_integer (new_stmt, 1) > 0 then
                  pattern_glob := True;
               end if;
               if SQLite.retrieve_integer (new_stmt, 2) > 0 then
                  pattern_regex := True;
               end if;
            when SQLite.something_else =>
               CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                            SQLite.get_expanded_sql (new_stmt));
            when SQLite.no_more_data => exit;
         end case;
      end loop;
      SQLite.finalize_statement (new_stmt);
      Event.emit_debug (high_level, "exact=" & pattern_exact'Img &
                          " glob=" & pattern_glob'Img &
                          " regex=" & pattern_regex'Img);
   end file_triggers_present;


   ------------------------------
   --  get_file_triggers_core  --
   ------------------------------
   procedure get_file_triggers_core
     (db        : Database.RDB_Connection;
      pkgid_set : String;
      sql_file  : String;
      sql_patt  : String;
      file_list : in out Directory_Triggers.Vector)
   is
      new_stmt : SQLite.thick_stmt;
      trg_stmt : SQLite.thick_stmt;
      func     : constant String := "get_file_triggers_core";

      type tmprec is
         record
            trig_id : Natural;
            script  : Text;
            pattern : Text;
         end record;

      package trig_jar is new Ada.Containers.Vectors
        (Element_Type => tmprec,
         Index_Type   => Natural);

      myjar : trig_jar.Vector;
   begin
      file_list.Clear;
      if not SQLite.prepare_sql (db.handle, sql_patt, trg_stmt) then
         CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func, sql_patt);
         return;
      end if;
      debug_running_stmt (trg_stmt);


      loop
         case SQLite.step (trg_stmt) is
            when SQLite.row_present =>
               declare
                  myrec : tmprec;
               begin
                  myrec.trig_id := Natural (SQLite.retrieve_integer (trg_stmt, 0));
                  myrec.script  := SUS (SQLite.retrieve_string (trg_stmt, 1));
                  myrec.pattern := SUS (SQLite.retrieve_string (trg_stmt, 2));
                  myjar.Append (myrec);
               end;
            when SQLite.something_else =>
               CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                            SQLite.get_expanded_sql (trg_stmt));
               exit;
            when SQLite.no_more_data => exit;
         end case;
      end loop;
      SQLite.finalize_statement (trg_stmt);

      if not SQLite.prepare_sql (db.handle, sql_file, new_stmt) then
         CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func, sql_file);
         return;
      end if;

      declare
         procedure assemble_trigger (Position : trig_jar.Cursor)
         is
            trigrec : tmprec renames trig_jar.Element (Position);
         begin
            if SQLite.reset_statement (new_stmt) then
               SQLite.bind_string (new_stmt, 1, USS (trigrec.pattern));
               debug_running_stmt (new_stmt);
               loop
                  case SQLite.step (new_stmt) is
                     when SQLite.something_else =>
                        CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                                  SQLite.get_expanded_sql (new_stmt));
                        exit;
                     when SQLite.no_more_data => exit;
                     when SQLite.row_present =>
                        declare
                           myrec : A_Directory_Trigger;
                        begin
                           myrec.ent_path := SUS (SQLite.retrieve_string (new_stmt, 1));
                           myrec.namebase := SUS (SQLite.retrieve_string (new_stmt, 2));
                           myrec.subpkg   := SUS (SQLite.retrieve_string (new_stmt, 3));
                           myrec.variant  := SUS (SQLite.retrieve_string (new_stmt, 4));
                           myrec.prefix   := SUS (SQLite.retrieve_string (new_stmt, 5));
                           myrec.script   := trigrec.script;
                           myrec.trig_id  := trigrec.trig_id;
                           myrec.pkg_id   := Pkgtypes.Package_ID (SQLite.retrieve_integer
                                                                  (new_stmt, 0));
                           file_list.Append (myrec);
                        end;
                  end case;
               end loop;
            else
               Event.emit_error ("Failed to reset inner query of " & func);
            end if;
         end assemble_trigger;
      begin
         myjar.Iterate (assemble_trigger'Access);
      end;
      SQLite.finalize_statement (new_stmt);
   end get_file_triggers_core;



   -------------------------------
   --  get_file_exact_triggers  --
   -------------------------------
   procedure get_file_exact_triggers
     (db        : Database.RDB_Connection;
      pkgid_set : String;
      file_list : in out Directory_Triggers.Vector)
   is
      sql_file : constant String :=
        "SELECT f.package_id, f.path, p.namebase, p.subpackage, p.variant, p.prefix " &
        "FROM pkg_files f " &
        "JOIN packages p on p.id = f.package_id " &
        "WHERE f.path = ? AND f.package_id IN " & pkgid_set;
      sql_patt : constant String :=
        "SELECT t.trigger_id, t.code, path.path_value " &
        "FROM pkg_triggers t " &
        "JOIN trigger_paths path on path.trigger_id = t.trigger_id " &
        "WHERE t.trigger_type = 1 AND path.path_type = 1";
   begin
      get_file_triggers_core (db, pkgid_set, sql_file, sql_patt, file_list);
   end get_file_exact_triggers;


   ------------------------------
   --  get_file_glob_triggers  --
   ------------------------------
   procedure get_file_glob_triggers
     (db        : Database.RDB_Connection;
      pkgid_set : String;
      file_list : in out Directory_Triggers.Vector)
   is
      sql_file : constant String :=
        "SELECT f.package_id, f.path, p.namebase, p.subpackage, p.variant, p.prefix " &
        "FROM pkg_files f " &
        "JOIN packages p on p.id = f.package_id " &
        "WHERE f.path GLOB ? AND f.package_id IN " & pkgid_set;
      sql_patt : constant String :=
        "SELECT t.trigger_id, t.code, path.path_value " &
        "FROM pkg_triggers t " &
        "JOIN trigger_paths path on path.trigger_id = t.trigger_id " &
        "WHERE t.trigger_type = 1 AND path.path_type = 2";
   begin
      get_file_triggers_core (db, pkgid_set, sql_file, sql_patt, file_list);
   end get_file_glob_triggers;


   --------------------------------
   --  get_file_regexp_triggers  --
   --------------------------------
   procedure get_file_regexp_triggers
     (db        : Database.RDB_Connection;
      pkgid_set : String;
      file_list : in out Directory_Triggers.Vector)
   is
      sql_file : constant String :=
        "SELECT f.package_id, f.path, p.namebase, p.subpackage, p.variant, p.prefix " &
        "FROM pkg_files f " &
        "JOIN packages p on p.id = f.package_id " &
        "WHERE f.path REGEXP ? AND f.package_id IN " & pkgid_set;
      sql_patt : constant String :=
        "SELECT t.trigger_id, t.code, path.path_value " &
        "FROM pkg_triggers t " &
        "JOIN trigger_paths path on path.trigger_id = t.trigger_id " &
        "WHERE t.trigger_type = 1 AND path.path_type = 3";
   begin
      get_file_triggers_core (db, pkgid_set, sql_file, sql_patt, file_list);
   end get_file_regexp_triggers;


end Raven.Database.Triggers;
