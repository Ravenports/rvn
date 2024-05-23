--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

package body Raven.Cmd.RQuery is

   ------------------------------
   --  execute_rquery_command  --
   ------------------------------
   function execute_rquery_command (comline : Cldata) return Boolean
   is
      mirrors : Repository.A_Repo_Config_Set;
      single  : constant String := Strings.USS (comline.common_options.repo_name);
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



      OPS.rdb_close (rdb);


      return False;
   end execute_rquery_command;

end Raven.Cmd.RQuery;
