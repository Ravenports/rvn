--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with SQLite;
with Raven.Event;
with Raven.Database.CommonSQL;

package body Raven.Database.Query is

   -------------------------
   --  package_installed  --
   -------------------------
   function package_installed
     (db         : in out RDB_Connection;
      namebase   : String;
      subpackage : String;
      variant    : String) return Pkgtypes.Package_ID
   is
      func : constant String := "package_installed";
      sql : constant String := "SELECT id FROM packages where namebase = ?1 " &
                               "AND subpackage = ?2 AND variant = ?3";
      new_stmt : SQLite.thick_stmt;
      installed : Pkgtypes.Package_ID := Pkgtypes.Package_Not_Installed;
   begin
      if not SQLite.prepare_sql (db.handle, sql, new_stmt) then
         CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func, sql);
         return installed;
      end if;
      SQLite.bind_string (new_stmt, 1, namebase);
      SQLite.bind_string (new_stmt, 2, subpackage);
      SQLite.bind_string (new_stmt, 3, variant);
      debug_running_stmt (new_stmt);

      case SQLite.step (new_stmt) is
         when SQLite.no_more_data => null;
         when SQLite.row_present =>
            installed := Pkgtypes.Package_ID (SQLite.retrieve_integer (new_stmt, 1));
         when SQLite.something_else =>
            CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                         SQLite.get_expanded_sql (new_stmt));
      end case;
      SQLite.finalize_statement (new_stmt);
      return installed;
   end package_installed;


end Raven.Database.Query;
