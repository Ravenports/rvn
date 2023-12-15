--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Cmd.Unset;
with Raven.Event;
with Raven.Configuration;
with Raven.Strings; Use Raven.Strings;

package body Raven.Cmd.Alias is

   package RCU renames Raven.Cmd.Unset;
   package CFG renames Raven.Configuration;

   -------------------------------
   --  execute_alias_command --
   -------------------------------
   function execute_alias_command (comline : Cldata) return Boolean
   is
   begin
      --  Handle full list
      if IsBlank (comline.cmd_alias.alias) then
         --  List all known aliases alphabetically
         if not comline.common_options.quiet then
            TIO.Put("ALIAS");
            if comline.cmd_alias.without_args then
               TIO.Put_Line("");
            else
               TIO.Put_Line("                     ARGUMENTS");
            end if;
         end if;
         declare
            delim   : constant Character := Character'Val (0);
            delim2  : constant String (1 .. 1) := (others => delim);
            setting : constant String := RCU.config_setting_as_string (CFG.alias);
            lines   : Natural := count_char (setting, delim) + 1;
         begin
            if not IsBlank (setting) then
               for field_number in 1 .. lines loop
                  declare
                     line : constant String := specific_field (setting, field_number, delim2);
                  begin
                     TIO.Put (pad_right (part_1 (line, ": "), 26));
                     if comline.cmd_alias.without_args then
                        TIO.Put_Line("");
                     else
                        TIO.Put_Line (part_2 (line, ": "));
                     end if;
                  end;
               end loop;
            end if;
         end;
         return True;
      end if;

      --  Handle specific aliases
      declare
         delim   : constant Character := Character'Val (0);
         delim2  : constant String (1 .. 1) := (others => delim);
         setting : constant String := USS (comline.cmd_alias.alias);
         words   : Natural := count_char (setting, delim) + 1;
         all_found : Boolean := True;
      begin
         for word in 1 .. words loop
            declare
               found : Boolean := False;
               alias_name : constant String := specific_field (setting, word, delim2);
               val : constant String := RCU.config_setting_map_value (CFG.alias, alias_name, found);
            begin
               if found then
                  TIO.Put_Line (val);
               else
                  Raven.Event.emit_error (val);
               end if;
               all_found := all_found and found;
            end;
         end loop;
         return all_found;
      end;
   end execute_alias_command;

end Raven.Cmd.Alias;
