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


   --------------------------------------------------------------------
   --  execute_command
   --------------------------------------------------------------------
   function execute_command (comline : Cldata) return Boolean is
   begin
      case comline.command is
         when cv_unset    => return (C00.execute_no_command (comline));
         when cv_help     => return (C01.execute_help_command (comline));
         when cv_create   => return (C02.execute_create_command (comline));
         when cv_config   => return (C03.execute_config_command (comline));
         when cv_alias    => return (C04.execute_alias_command (comline));
         when cv_info     => return (C05.execute_info_command (comline));
         when cv_install  => return (C06.execute_install_command (comline));
         when cv_shell    => return (C07.execute_shell_command (comline));
         when cv_shlib    => return (C08.execute_shlib_command (comline));
         when cv_which    => return (C09.execute_which_command (comline));
         when cv_version  => return (C10.execute_version_command (comline));
         when cv_genrepo  => return (C11.execute_genrepo_command (comline));
         when cv_catalog  => return (C12.execute_catalog_command (comline));
         when others =>
            TIO.Put_Line ("Command '" & convert_command_enum_to_label (comline.command) &
                            "' hasn't been implemented yet.  Sorry!");
            return False;
      end case;
   end execute_command;

end Raven.Cmd.Bahnhof;
