--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Event;
with Raven.Database.Lock;
with Raven.Database.UserQuery;
with Raven.Database.Operations;
with Raven.Strings; use Raven.Strings;


package body Raven.Cmd.Query is

   package LOK renames Raven.Database.Lock;
   package DUC renames Raven.Database.UserQuery;
   package OPS renames Raven.Database.Operations;

   -----------------------------
   --  execute_query_command  --
   -----------------------------
   function execute_query_command (comline : Cldata) return Boolean
   is
      success : Boolean;
      rdb     : Database.RDB_Connection;
   begin
      case OPS.rdb_open_localdb (rdb, Database.installed_packages) is
         when RESULT_OK => null;
         when others => return False;
      end case;

      if not LOK.obtain_lock (rdb, LOK.lock_readonly) then
         Event.emit_error (LOK.no_read_lock);
         OPS.rdb_close (rdb);
         return False;
      end if;

      success := DUC.query_package_database
        (db             => rdb,
         selection      => USS (comline.cmd_query.query_format),
         conditions     => USS (comline.cmd_query.evaluate),
         pattern        => USS (comline.common_options.name_pattern),
         all_packages   => comline.common_options.all_installed_pkgs,
         override_exact => comline.common_options.exact_match);

      if not LOK.release_lock (rdb, LOK.lock_readonly) then
         Event.emit_error (LOK.no_read_unlock);
         OPS.rdb_close (rdb);
         return False;
      end if;

      OPS.rdb_close (rdb);
      return success;
   end execute_query_command;

end Raven.Cmd.Query;
