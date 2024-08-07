--  SPDX-License-Identifier: ISC
--  Reference: /License.txt


with Raven.Event;
with Raven.Repository;
with Raven.Database.Lock;
with Raven.Database.UserQuery;
with Raven.Database.Operations;
with Raven.Strings; use Raven.Strings;
with Archive.Unix;


package body Raven.Cmd.RQuery is

   package LOK renames Raven.Database.Lock;
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
      rdb     : Database.RDB_Connection;
   begin
      if Archive.Unix.user_is_root then
         Repository.load_repository_configurations (mirrors, single);
         if not Repository.create_local_catalog_database
           (remote_repositories  => mirrors,
            forced               => False,
            quiet                => comline.common_options.quiet)
         then
            Event.emit_error ("Failed to update the local catalog");
         end if;
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

      if not LOK.obtain_lock (rdb, LOK.lock_readonly) then
         Event.emit_error (LOK.no_read_lock);
         OPS.rdb_close (rdb);
         return False;
      end if;

      success := DUC.query_package_database
        (db             => rdb,
         selection      => USS (comline.cmd_rquery.query_format),
         conditions     => USS (comline.cmd_rquery.evaluate),
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

   end execute_rquery_command;

end Raven.Cmd.RQuery;
