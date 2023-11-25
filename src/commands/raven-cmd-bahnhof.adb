--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Raven.Cmd.Unset;

package body Raven.Cmd.Bahnhof is

   package C00 renames Raven.Cmd.Unset;

   --------------------------------------------------------------------
   --  execute_command
   --------------------------------------------------------------------
   function execute_command (comline : Cldata) return Boolean is
   begin
      case comline.command is
         when cv_unset |
              cv_help  => null;
         when others =>
            if not C00.initialize_program (comline) then
               return False;
            end if;
      end case;

      case comline.command is
         when cv_unset    =>
            return (C00.execute_no_command (comline));
         --  when cv_help     => return (C01.execute_help_command (comline));
         --  when cv_version  => return (C02.execute_version_command (comline));
         when others =>
            TIO.Put_Line ("Command '" & convert_command_enum_to_label (comline.command) &
                            "' hasn't been implemented yet.  Sorry!");
            return False;
      end case;
   end execute_command;

end Raven.Cmd.Bahnhof;
