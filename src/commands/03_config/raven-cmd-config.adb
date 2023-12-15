--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Cmd.Unset;
with Raven.Event;
with Raven.Configuration;
with Raven.Strings; Use Raven.Strings;

package body Raven.Cmd.Config is

   package RCU renames Raven.Cmd.Unset;
   package CFG renames Raven.Configuration;

   -------------------------------
   --  Executes config command  --
   -------------------------------
   function execute_config_command (comline : Cldata) return Boolean
   is
      CONFIG_KEY : constant String := uppercase (USS (comline.cmd_config.key));
      citem      : CFG.Configuration_Item := CFG.ci_not_found;
   begin
      for ci in CFG.Configuration_Item'Range loop
         if CFG.get_ci_key (ci) = CONFIG_KEY then
            citem := ci;
            exit;
         end if;
      end loop;

      case citem is
         when CFG.ci_not_found =>
            Raven.Event.emit_error
              (progname & ": " & CONFIG_KEY & " is not a recognized configuration option.");
         when CFG.alias | CFG.environ | CFG.repos_dir | CFG.valid_scheme =>
            declare
               delim   : constant Character := Character'Val (0);
               delim2  : constant String (1 .. 1) := (others => delim);
               setting : constant String := RCU.config_setting_as_string (citem);
               lines   : Natural := count_char (setting, delim) + 1;
            begin
               for line in 1 .. lines loop
                  TIO.Put_Line (specific_field (setting, line, delim2));
               end loop;
            end;
         when others =>
            TIO.Put_Line (RCU.config_setting_as_string (citem));
      end case;

      return True;
   end execute_config_command;

end Raven.Cmd.Config;
