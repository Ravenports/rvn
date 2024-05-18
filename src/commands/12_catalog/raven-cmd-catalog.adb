--  SPDX-License-Identifier: ISC
--  Reference: /License.txt


with Raven.Repository;
with Raven.Strings;

package body Raven.Cmd.Catalog is

   -------------------------------
   --  execute_catalog_command  --
   -------------------------------
   function execute_catalog_command (comline : Cldata) return Boolean
   is
      mirrors : Repository.A_Repo_Config_Set;
      single  : constant String := Strings.USS (comline.common_options.repo_name);
   begin
      Repository.load_repository_configurations (mirrors, single);
      return Repository.create_local_catalog_database
        (remote_repositories  => mirrors,
         forced               => comline.cmd_catalog.force_update,
         quiet                => comline.common_options.quiet,
         from_catalog_command => True);

   end execute_catalog_command;

end Raven.Cmd.Catalog;
