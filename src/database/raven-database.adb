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

end Raven.Database;
