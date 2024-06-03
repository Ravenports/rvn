--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Cmd.Unset;
with Raven.Cmd.Alias;
with Raven.Cmd.Config;
with Raven.Cmd.Create;
with Raven.Cmd.Help;
with Raven.Cmd.Info;
with Raven.Cmd.Install;
with Raven.Cmd.Shell;
with Raven.Cmd.Shlib;
with Raven.cmd.Version;
with Raven.Cmd.Which;
with Raven.Cmd.Genrepo;
with Raven.Cmd.Catalog;
with Raven.Cmd.Clean;
with Raven.Cmd.Query;
with Raven.Cmd.RQuery;
with Raven.Cmd.Stats;
with Raven.Cmd.Search;
with Raven.Cmd.Fetch;
with Raven.Cmd.Remove;
with Raven.Cmd.Autoremove;
with Raven.Cmd.Check;

package body Raven.Cmd.Bahnhof is

   package C00 renames Raven.Cmd.Unset;
   package C01 renames Raven.Cmd.Help;
   package C02 renames Raven.Cmd.Create;
   package C03 renames Raven.Cmd.Config;
   package C04 renames Raven.Cmd.Alias;
   package C05 renames Raven.Cmd.Info;
   package C06 renames Raven.Cmd.Install;
   package C07 renames Raven.Cmd.Shell;
   package C08 renames Raven.Cmd.Shlib;
   package C09 renames Raven.Cmd.Which;
   package C10 renames Raven.Cmd.Version;
   package C11 renames Raven.Cmd.Genrepo;
   package C12 renames Raven.Cmd.Catalog;
   package C13 renames Raven.Cmd.Clean;
   package C14 renames Raven.Cmd.Query;
   package C15 renames Raven.Cmd.RQuery;
   package C16 renames Raven.Cmd.Stats;
   package C17 renames Raven.Cmd.Search;
   package C18 renames Raven.Cmd.Fetch;
   package C19 renames Raven.Cmd.Remove;
   package C20 renames Raven.Cmd.Autoremove;
   package C21 renames Raven.Cmd.Check;


   --------------------------------------------------------------------
   --  execute_command
   --------------------------------------------------------------------
   function execute_command (comline : Cldata) return Boolean is
   begin
      case comline.command is
         when cv_unset      => return (C00.execute_no_command (comline));
         when cv_help       => return (C01.execute_help_command (comline));
         when cv_create     => return (C02.execute_create_command (comline));
         when cv_config     => return (C03.execute_config_command (comline));
         when cv_alias      => return (C04.execute_alias_command (comline));
         when cv_info       => return (C05.execute_info_command (comline));
         when cv_install    => return (C06.execute_install_command (comline));
         when cv_shell      => return (C07.execute_shell_command (comline));
         when cv_shlib      => return (C08.execute_shlib_command (comline));
         when cv_which      => return (C09.execute_which_command (comline));
         when cv_version    => return (C10.execute_version_command (comline));
         when cv_genrepo    => return (C11.execute_genrepo_command (comline));
         when cv_catalog    => return (C12.execute_catalog_command (comline));
         when cv_clean      => return (C13.execute_clean_command (comline));
         when cv_query      => return (C14.execute_query_command (comline));
         when cv_rquery     => return (C15.execute_rquery_command (comline));
         when cv_stats      => return (C16.execute_stats_command (comline));
         when cv_search     => return (C17.execute_search_command (comline));
         when cv_fetch      => return (C18.execute_fetch_command (comline));
         when cv_remove     => return (C19.execute_remove_command (comline));
         when cv_autoremove => return (C20.execute_autoremove_command (comline));
         when cv_check      => return (C21.execute_check_command (comline));
      end case;
   end execute_command;

end Raven.Cmd.Bahnhof;
