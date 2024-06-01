--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Database.UserQuery;
with Raven.Database.Operations;
with Raven.Strings; use Raven.Strings;


package body Raven.Cmd.Query is

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

      success := DUC.query_package_database
        (db             => rdb,
         selection      => USS (comline.cmd_query.query_format),
         conditions     => USS (comline.cmd_query.evaluate),
         pattern        => USS (comline.common_options.name_pattern),
         all_packages   => comline.common_options.all_installed_pkgs,
         override_exact => comline.common_options.exact_match);

      OPS.rdb_close (rdb);
      return success;
   end execute_query_command;

end Raven.Cmd.Query;
