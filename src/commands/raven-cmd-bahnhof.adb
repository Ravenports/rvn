--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Raven.Cmd.Unset;
with Raven.Cmd.Alias;
with Raven.Cmd.Config;
with Raven.Cmd.Create;
with Raven.Cmd.Help;

package body Raven.Cmd.Bahnhof is

   package C00 renames Raven.Cmd.Unset;
   package C01 renames Raven.Cmd.Help;
   package C02 renames Raven.Cmd.Create;
   package C03 renames Raven.Cmd.Config;
   package C04 renames Raven.Cmd.Alias;

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
         when others =>
            TIO.Put_Line ("Command '" & convert_command_enum_to_label (comline.command) &
                            "' hasn't been implemented yet.  Sorry!");
            return False;
      end case;
   end execute_command;

end Raven.Cmd.Bahnhof;
