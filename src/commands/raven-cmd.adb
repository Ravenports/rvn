--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt


with Ada.Text_IO;

package body Raven.Cmd is

   -------------------------------------
   --  convert_command_enum_to_label  --
   -------------------------------------
   function convert_command_enum_to_label (command : Command_verb) return String
   is
   begin
      case command is
         when cv_unset      => return "";
         when cv_alias      => return "alias";
         when cv_config     => return "config";
         when cv_create     => return "create";
         when cv_info       => return "info";
         when cv_help       => return "help";
         when cv_install    => return "install";
         --  when cv_annotate   => return "annotate";
         --  when cv_autoremove => return "autoremove";
         --  when cv_check      => return "check";
         --  when cv_clean      => return "clean";
      end case;
   end convert_command_enum_to_label;

end Raven.Cmd;
