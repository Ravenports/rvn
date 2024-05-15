--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Event;

package body Raven.Database is

   --------------------------
   --  debug_running_stmt  --
   --------------------------
   procedure debug_running_stmt (stmt : SQLite.thick_stmt) is
   begin
      Event.emit_debug (high_level, "[rdb] running: " & SQLite.get_expanded_sql (stmt));
   end debug_running_stmt;


   -------------------------
   --  database_filename  --
   -------------------------
   function database_filename (contents : RDB_Contents) return String
   is
   begin
      case contents is
         when installed_packages => return localhost_database;
         when catalog            => return localhost_catalog;
      end case;
   end database_filename;


end Raven.Database;
