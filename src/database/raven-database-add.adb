--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Event;
with Raven.Context;
with Raven.Strings;
with Raven.Pkgtypes;
with Raven.Cmd.Unset;
with Raven.Database.Query;
with Raven.Database.CommonSQL;

use Raven.Strings;
use Raven.Pkgtypes;

package body Raven.Database.Add is

   package QRY renames Raven.Database.Query;
   package RCU renames Raven.Cmd.Unset;


   -------------------------------
   --  top_level_addition_list  --
   -------------------------------
   function top_level_addition_list
     (db             : RDB_Connection;
      packages       : in out Pkgtypes.Package_Set.Vector;
      pattern        : String;
      override_exact : Boolean;
      select_all     : Boolean) return Boolean
   is
      leading_match : Boolean := False;
      success : Boolean := True;
      func    : constant String := "top_level_addition_list";
      sqlbase : constant String := "SELECT " & nsv_formula & " as nsv000, " &
        "namebase, subpackage, variant, version, comment, desc, www, maintainer, prefix, " &
        "abi, rvndigest, rvnsize, flatsize, licenselogic, id, automatic " &
        "FROM packages as p";
      new_stmt : SQLite.thick_stmt;
      sql : Text := SUS (sqlbase);
   begin
      if not select_all then
         if override_exact then
            sql := SUS (sqlbase & " WHERE nsv000 = ?");
         elsif Context.reveal_case_sensitive then
            sql := SUS (sqlbase & " WHERE nsv000 GLOB ?");
         else
            sql := SUS (sqlbase & " WHERE nsv000 LIKE ?");
            leading_match := True;
         end if;
      end if;

      if not SQLite.prepare_sql (db.handle, USS (sql), new_stmt) then
         Database.CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func, USS (sql));
         return False;
      end if;

      if not select_all then
         if leading_match then
            SQLite.bind_string (new_stmt, 1, pattern & '%');
         else
            SQLite.bind_string (new_stmt, 1, pattern);
         end if;
      end if;
      debug_running_stmt (new_stmt);

      loop
         case SQLite.step (new_stmt) is
            when SQLite.no_more_data => exit;
            when SQLite.row_present =>
               declare
                  myrec : Pkgtypes.A_Package;
                  subpackage : constant String := SQLite.retrieve_string (new_stmt, 2);
               begin
                  if allow_addition (subpackage, override_exact) then
                     myrec.namebase   := SUS (SQLite.retrieve_string (new_stmt, 1));
                     myrec.subpackage := SUS (subpackage);
                     myrec.variant    := SUS (SQLite.retrieve_string (new_stmt, 3));
                     myrec.version    := SUS (SQLite.retrieve_string (new_stmt, 4));
                     myrec.comment    := SUS (SQLite.retrieve_string (new_stmt, 5));
                     myrec.desc       := SUS (SQLite.retrieve_string (new_stmt, 6));
                     myrec.www        := SUS (SQLite.retrieve_string (new_stmt, 7));
                     myrec.maintainer := SUS (SQLite.retrieve_string (new_stmt, 8));
                     myrec.prefix     := SUS (SQLite.retrieve_string (new_stmt, 9));
                     myrec.abi        := SUS (SQLite.retrieve_string (new_stmt, 10));
                     myrec.rvndigest  := SUS (SQLite.retrieve_string (new_stmt, 11));
                     myrec.rvnsize    := Package_Size (SQLite.retrieve_integer (new_stmt, 12));
                     myrec.flatsize   := Package_Size (SQLite.retrieve_integer (new_stmt, 13));
                     myrec.licenselogic := License_Logic'Val (SQLite.retrieve_integer
                                                              (new_stmt, 14));
                     myrec.id         := Package_ID (SQLite.retrieve_integer (new_stmt, 15));
                     myrec.automatic  := SQLite.retrieve_boolean (new_stmt, 16);

                     QRY.finish_package_dependencies (db, myrec, True);
                     QRY.finish_package_libs_provided (db, myrec);
                     QRY.finish_package_libs_required (db, myrec);
                     QRY.finish_package_files (db, myrec);
                     packages.Append (myrec);
                     Event.emit_debug (high_level, "Added to top-level match for addition: " &
                                         Pkgtypes.nsv_identifier (myrec));
                  end if;
               end;
            when SQLite.something_else =>
               CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                            SQLite.get_expanded_sql (new_stmt));
               success := False;
               exit;
         end case;
      end loop;
      SQLite.finalize_statement (new_stmt);
      return success;

   end top_level_addition_list;


   ----------------------
   --  allow_addition  --
   ----------------------
   function allow_addition (subpackage : String; override_exact : Boolean) return Boolean is
   begin
      if override_exact then
         return True;
      end if;
      return not RCU.subpackage_type_banned (subpackage);
   end;


   ---------------------------------------------
   --  gather_packages_affected_by_libchange  --
   ---------------------------------------------
   procedure gather_packages_affected_by_libchange
     (db             : RDB_Connection;
      old_library    : String;
      affected_list  : in out Pkgtypes.Text_List.Vector)
   is
      func     : constant String := "gather_packages_affected_by_libchange";
      new_stmt : SQLite.thick_stmt;
      sql      : constant String :=
        "SELECT " & nsv_formula & " as nsv000 " &
        "FROM packages p " &
        "JOIN pkg_libs_required x ON x.package_id = p.id " &
        "JOIN libraries ml on ml.library_id = x.library_id " &
        "WHERE ml.name = ?";
   begin
      affected_list.Clear;
      if not SQLite.prepare_sql (db.handle, sql, new_stmt) then
         Database.CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func, sql);
         return;
      end if;
      debug_running_stmt (new_stmt);
      loop
         case SQLite.step (new_stmt) is
            when SQLite.no_more_data => exit;
            when SQLite.row_present =>
               declare
                  nsv : constant String := SQLite.retrieve_string (new_stmt, 0);
               begin
                  affected_list.Append (SUS (nsv));
               end;
            when SQLite.something_else =>
               CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                            SQLite.get_expanded_sql (new_stmt));
               exit;
         end case;
      end loop;
      SQLite.finalize_statement (new_stmt);
   end gather_packages_affected_by_libchange;


   -------------------------------
   --  collect_installed_files  --
   -------------------------------
   procedure collect_installed_files
     (db             : RDB_Connection;
      new_upgrades   : Pkgtypes.Text_List.Vector;
      collection     : in out Pkgtypes.NV_Pairs.Map)
   is
      func     : constant String := "gather_packages_affected_by_libchange";
      new_stmt : SQLite.thick_stmt;
      sqlbase  : constant String :=
        "SELECT " & nsv_formula & " as nsv, ml.path " &
        "FROM packages p " &
        "JOIN pkg_files ml ON ml.package_id = p.id " &
        "WHERE nsv NOT IN ('nada'";
      sql : Text := SUS (sqlbase);
      column : Natural := 0;

      procedure build_up (Position : Pkgtypes.Text_List.Cursor) is
      begin
         SU.Append (sql, ", ?");
      end build_up;

      procedure bind_up (Position : Pkgtypes.Text_List.Cursor)
      is
         nsv : constant String := USS (Pkgtypes.Text_List.Element (Position));
      begin
         column := column + 1;
         SQLite.bind_string (new_stmt, column, nsv);
      end bind_up;
   begin
      collection.Clear;
      new_upgrades.Iterate (build_up'Access);
      SU.Append (sql, ")");

      if not SQLite.prepare_sql (db.handle, USS (sql), new_stmt) then
         Database.CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func, USS (sql));
         return;
      end if;

      new_upgrades.Iterate (bind_up'Access);
      debug_running_stmt (new_stmt);
      loop
         case SQLite.step (new_stmt) is
            when SQLite.no_more_data => exit;
            when SQLite.row_present =>
               declare
                  nsv  : constant String := SQLite.retrieve_string (new_stmt, 0);
                  path : constant String := SQLite.retrieve_string (new_stmt, 1);
               begin
                  collection.Insert (SUS (path), SUS (nsv));
               end;
            when SQLite.something_else =>
               CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                            SQLite.get_expanded_sql (new_stmt));
               exit;
         end case;
      end loop;
      SQLite.finalize_statement (new_stmt);
   end collect_installed_files;


end Raven.Database.Add;
