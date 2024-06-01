--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Context;
with Raven.Strings;
with Raven.Pkgtypes;
with Raven.Database.CommonSQL;

use Raven.Strings;
use Raven.Pkgtypes;

package body Raven.Database.Remove is


   -------------------------------
   --  top_level_deletion_list  --
   -------------------------------
   function top_level_deletion_list
     (db             : in out RDB_Connection;
      packages       : in out Pkgtypes.Package_Set.Vector;
      pattern        : String;
      all_packages   : Boolean;
      override_exact : Boolean;
      force          : Boolean) return Boolean
   is
      leading_match : Boolean := False;
      func    : constant String := "top_level_deletion_list";
      sqlbase : constant String := "SELECT " & nsv_formula & " as nsv000, " &
        "namebase, subpackage, variant, version, comment, desc, www, maintainer, prefix, " &
        "abi, rvndigest, rvnsize, flatsize, licenselogic, id " &
        "FROM packages as p";
      new_stmt : SQLite.thick_stmt;
      sql : Text;
   begin
      if all_packages then
         if force then
            sql := SUS (sqlbase);
         else
            sql := SUS (sqlbase & " WHERE nsv000 NOT GLOB 'rvn-*-standard'");
         end if;
      else
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

      if not all_packages then
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
               begin
                  myrec.namebase   := SUS (SQLite.retrieve_string (new_stmt, 1));
                  myrec.subpackage := SUS (SQLite.retrieve_string (new_stmt, 2));
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
                  myrec.licenselogic := License_Logic'Val (SQLite.retrieve_integer (new_stmt, 14));
                  myrec.id         := Package_ID (SQLite.retrieve_integer (new_stmt, 15));
               end;
            when SQLite.something_else =>
               CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                            SQLite.get_expanded_sql (new_stmt));
         end case;
      end loop;
      SQLite.finalize_statement (new_stmt);
      return True;

   end top_level_deletion_list;

end Raven.Database.Remove;
