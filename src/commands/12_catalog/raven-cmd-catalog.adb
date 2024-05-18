--  SPDX-License-Identifier: ISC
--  Reference: /License.txt


with Raven.Repository;

package body Raven.Cmd.Catalog is

   -------------------------------
   --  execute_catalog_command  --
   -------------------------------
   function execute_catalog_command (comline : Cldata) return Boolean
   is
      mirrors : Repository.A_Repo_Config_Set;
   begin
      Repository.load_repository_configurations (mirrors);
      return Repository.create_local_catalog_database (mirrors);
   end execute_catalog_command;

end Raven.Cmd.Catalog;
