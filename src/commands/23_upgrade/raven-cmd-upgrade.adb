--  SPDX-License-Identifier: ISC
--  Reference: /License.txt


with Raven.Event;
with Raven.Repository;
with Raven.Database;
with Raven.Strings;
with Raven.Install;
with Archive.Unix;

use Raven.Strings;

package body Raven.Cmd.Upgrade is

   -------------------------------
   --  execute_upgrade_command  --
   -------------------------------
   function execute_upgrade_command (comline : Cldata) return Boolean
   is
      rdb     : Database.RDB_Connection;
      mirrors : Repository.A_Repo_Config_Set;
      single  : constant String := USS (comline.common_options.repo_name);
   begin
      if Archive.Unix.user_is_root then
         Repository.load_repository_configurations (mirrors, single);
         if not Repository.create_local_catalog_database
           (remote_repositories  => mirrors,
            forced               => False,
            quiet                => True)
         then
            Event.emit_error ("Failed to update the local catalog");
         end if;
      end if;

      return Raven.Install.upgrade_installed_packages
        (opt_exact_match  => comline.common_options.exact_match,
         opt_quiet        => comline.common_options.quiet,
         opt_force        => comline.cmd_install.force_install,
         opt_skip_scripts => comline.cmd_install.inhibit_scripts,
         opt_dry_run      => comline.common_options.dry_run,
         opt_fetch_only   => comline.cmd_install.fetch_only,
         single_repo      => single,
         rootdir          => USS (comline.pre_command.install_rootdir),
         patterns         => comline.cmd_install.name_patterns);

   end execute_upgrade_command;

end Raven.Cmd.Upgrade;
