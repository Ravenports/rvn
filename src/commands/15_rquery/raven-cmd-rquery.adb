--  SPDX-License-Identifier: ISC
--  Reference: /License.txt


with Raven.Event;
with Raven.Repository;
with Raven.Database.UserQuery;
with Raven.Database.Operations;
with Raven.Strings; use Raven.Strings;


package body Raven.Cmd.RQuery is

   package DUC renames Raven.Database.UserQuery;
   package OPS renames Raven.Database.Operations;


   ------------------------------
   --  execute_rquery_command  --
   ------------------------------
   function execute_rquery_command (comline : Cldata) return Boolean
   is
      mirrors : Repository.A_Repo_Config_Set;
      single  : constant String := Strings.USS (comline.common_options.repo_name);
      success : Boolean;
   begin
      Repository.load_repository_configurations (mirrors, single);
      if not Repository.create_local_catalog_database
        (remote_repositories  => mirrors,
         forced               => False,
         quiet                => True)
      then
         Event.emit_error ("Failed to update the local catalog");
      end if;

      if not OPS.localdb_exists (Database.catalog) then
         Event.emit_error
           ("Catalog database is missing, should be here: " & OPS.localdb_path (Database.catalog));
         return False;
      end if;

      case OPS.rdb_open_localdb (rdb, Database.catalog) is
         when RESULT_OK => null;
         when others => return False;
      end case;

      success := DUC.query_package_database
        (db             => rdb,
         selection      => USS (comline.cmd_rquery.query_format),
         conditions     => USS (comline.cmd_rquery.evaluate),
         pattern        => USS (comline.common_options.name_pattern),
         all_packages   => comline.common_options.all_installed_pkgs,
         override_exact => comline.common_options.exact_match);

      OPS.rdb_close (rdb);
      return success;

   end execute_rquery_command;

end Raven.Cmd.RQuery;
