--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Event;
with Raven.Database.Operations;
with Raven.Database.CommonSQL;
with Raven.Database.Schema;
with Raven.Database.Pkgs;
with Raven.Strings;
with SQLite;

use Raven.Strings;

package body Raven.Database.Annotate is

   package OPS renames Raven.Database.Operations;
   package SCH renames Raven.Database.Schema;


   --------------------------
   --  commit_or_rollback  --
   --------------------------
   procedure commit_or_rollback
     (db        : RDB_Connection;
      revert    : Boolean;
      func      : String;
      savepoint : String)
   is
   begin
      if revert then
         if CommonSQL.transaction_rollback (db.handle, internal_srcfile, func, savepoint) then
            Event.emit_error ("Rolled back " & func & " transaction");
         end if;
      else
         if not CommonSQL.transaction_commit (db.handle, internal_srcfile, func, savepoint) then
            Event.emit_error ("Failed to commit " & func & " transaction");
            if CommonSQL.transaction_rollback (db.handle, internal_srcfile, func, savepoint) then
               Event.emit_error ("Rolled back " & func & " transaction");
            end if;
         end if;
      end if;
   end commit_or_rollback;


   -------------------------
   --  annotate_packages  --
   -------------------------
   procedure annotate_packages
     (db       : in out RDB_Connection;
      tag      : String;
      note     : String;
      packages : Pkgtypes.Package_Set.Vector)
   is
      del_stmt  : SQLite.thick_stmt;
      func      : constant String := "annotate_packages";
   begin
      if not SQLite.prepare_sql (db.handle, delsql, del_stmt) then
         CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func, delsql);
         return;
      end if;

      annotate_packages_core (db, tag, note, packages, del_stmt);
      SQLite.finalize_statement (del_stmt);

   end annotate_packages;


   ------------------------------
   --  annotate_packages_core  --
   ------------------------------
   procedure annotate_packages_core
     (db       : in out RDB_Connection;
      tag      : String;
      note     : String;
      packages : Pkgtypes.Package_Set.Vector;
      del_stmt : in out SQLite.thick_stmt)
   is
      func      : constant String := "annotate_packages_core";
      savepoint : constant String := "ANNOTATE";
      num_pkgs  : constant Natural := Natural (packages.Length);
      revert    : Boolean := False;

      main_stmt : SQLite.thick_stmt renames db.prepared_statements (Database.note);
      pack_stmt : SQLite.thick_stmt renames db.prepared_statements (Database.pkg_note);
   begin
      if not CommonSQL.transaction_begin (db.handle, internal_srcfile, func, savepoint) then
         Event.emit_error ("Failed to start transaction at " & func);
         return;
      end if;

      --  define tag if it doesn't already exist
      if SQLite.reset_statement (main_stmt) then
         SQLite.bind_string (main_stmt, 1, tag);
         debug_running_stmt (main_stmt);
         case SQLite.step (main_stmt) is
            when SQLite.no_more_data => null;
            when SQLite.row_present | SQLite.something_else =>
               CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                            SQLite.get_expanded_sql (main_stmt));
               revert := True;
         end case;
      end if;

      --  Remove any current definitions of tag
      if not revert then
         for pkg_index in 0 .. num_pkgs - 1 loop
            if SQLite.reset_statement (del_stmt) then
               SQLite.bind_integer (del_stmt, 1, SQLite.sql_int64 (packages (pkg_index).id));
               SQLite.bind_string  (del_stmt, 2, tag);
               debug_running_stmt (del_stmt);
               case SQLite.step (del_stmt) is
                  when SQLite.no_more_data => null;
                  when SQLite.row_present | SQLite.something_else =>
                     CommonSQL.ERROR_STMT_SQLITE
                       (db.handle, internal_srcfile, func, SQLite.get_expanded_sql (del_stmt));
                     revert := True;
                     exit;
               end case;
            else
               Event.emit_error ("Failed to reset pkg_annotation delete stmt");
               revert := True;
               exit;
            end if;
         end loop;
      end if;

      --  Insert custom tags
      if not revert then
         for pkg_index in 0 .. num_pkgs - 1 loop
            if SQLite.reset_statement (pack_stmt) then
               SQLite.bind_integer (pack_stmt, 1, SQLite.sql_int64 (packages (pkg_index).id));
               SQLite.bind_string  (pack_stmt, 2, tag);
               SQLite.bind_string  (pack_stmt, 3, note);
               SQLite.bind_integer (pack_stmt, 4, 1);  --  custom tags
               debug_running_stmt (pack_stmt);
               case SQLite.step (pack_stmt) is
                  when SQLite.no_more_data => null;
                  when SQLite.row_present | SQLite.something_else =>
                     CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                                  SQLite.get_expanded_sql (pack_stmt));
                     revert := True;
               end case;
            else
               Event.emit_error ("Failed to reset pkg_annotation insert stmt");
               revert := True;
               exit;
            end if;
         end loop;
      end if;

      commit_or_rollback (db, revert, func, savepoint);

   end annotate_packages_core;


   --------------------------
   --  remove_annotations  --
   --------------------------
   procedure remove_annotations
     (db       : RDB_Connection;
      tag      : String;
      packages : Pkgtypes.Package_Set.Vector)
   is
      del_stmt  : SQLite.thick_stmt;
      func   : constant String := "remove_annotations";
   begin
      if not SQLite.prepare_sql (db.handle, delsql, del_stmt) then
         CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func, delsql);
         return;
      end if;

      remove_annotations_core (db, tag, packages, del_stmt);
      SQLite.finalize_statement (del_stmt);
   end remove_annotations;


   -------------------------------
   --  remove_annotations_core  --
   -------------------------------
   procedure remove_annotations_core
     (db       : RDB_Connection;
      tag      : String;
      packages : Pkgtypes.Package_Set.Vector;
      del_stmt : in out SQLite.thick_stmt)
   is
      func      : constant String := "remove_annotations_core";
      savepoint : constant String := "DEANNOTATE";
      num_pkgs  : constant Natural := Natural (packages.Length);
      revert    : Boolean := False;
   begin
      if not CommonSQL.transaction_begin (db.handle, internal_srcfile, func, savepoint) then
         Event.emit_error ("Failed to start transaction at " & func);
         return;
      end if;

      for pkg_index in 0 .. num_pkgs - 1 loop
         if SQLite.reset_statement (del_stmt) then
            SQLite.bind_integer (del_stmt, 1, SQLite.sql_int64 (packages (pkg_index).id));
            SQLite.bind_string  (del_stmt, 2, tag);
            debug_running_stmt (del_stmt);
            case SQLite.step (del_stmt) is
               when SQLite.no_more_data => null;
               when SQLite.row_present | SQLite.something_else =>
                  CommonSQL.ERROR_STMT_SQLITE
                    (db.handle, internal_srcfile, func, SQLite.get_expanded_sql (del_stmt));
                  revert := True;
                  exit;
            end case;
         else
            Event.emit_error ("Failed to reset pkg_annotation delete stmt");
            revert := True;
         end if;
      end loop;

      commit_or_rollback (db, revert, func, savepoint);
   end remove_annotations_core;


   --------------------------------
   --  acquire_base_annotations  --
   --------------------------------
   procedure acquire_base_annotations
     (db       : RDB_Connection;
      notes    : in out base_note_set.Vector)
   is
      new_stmt : SQLite.thick_stmt;
      func : constant String := "acquire_base_annotations";
      sql : constant String :=
        "SELECT p.namebase, p.variant, p.version, k.note_key, n.annotation " &
        "FROM packages p " &
        "JOIN pkg_annotations n ON p.id = n.package_id " &
        "JOIN annotations k ON k.annotation_id = n.annotation_id " &
        "WHERE n.custom = 0 " &
        "GROUP BY p.namebase, p.variant " &
        "ORDER by k.note_key, n.annotation, p.namebase";
   begin
      notes.Clear;
      if not SQLite.prepare_sql (db.handle, sql, new_stmt) then
         CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func, sql);
         return;
      end if;
      debug_running_stmt (new_stmt);
      loop
         case SQLite.step (new_stmt) is
            when SQLite.no_more_data => exit;
            when SQLite.row_present =>
               declare
                  rec : base_note;
               begin
                  rec.namebase := SUS (SQLite.retrieve_string (new_stmt, 0));
                  rec.variant  := SUS (SQLite.retrieve_string (new_stmt, 1));
                  rec.version  := SUS (SQLite.retrieve_string (new_stmt, 2));
                  rec.note_key := SUS (SQLite.retrieve_string (new_stmt, 3));
                  rec.note_val := SUS (SQLite.retrieve_string (new_stmt, 4));
                  notes.Append (rec);
               end;
            when SQLite.something_else =>
               CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                            SQLite.get_expanded_sql (new_stmt));
         end case;
      end loop;
      SQLite.finalize_statement (new_stmt);
   end acquire_base_annotations;


end Raven.Database.Annotate;
