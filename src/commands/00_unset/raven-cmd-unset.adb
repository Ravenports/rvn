--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Raven.Strings; use Raven.Strings;

package body Raven.Cmd.Unset is

   --------------------------
   --  execute_no_command  --
   --------------------------
   function execute_no_command (comline : Cldata) return Boolean is
   begin
      case comline.unset_version is
         when not_shown =>
            --  switch --list
            if comline.unset_list_cmd then     
               return list_available_commands;
            end if;
            if comline.unset_status_check then
               if not initialize_program (comline) then
                  return False;
               end if;
               return do_status_check;
            end if;
            return False;
         when just_version =>
            --  switch -v
            return basic_version_info;          
         when dump_configuration =>
            --  switch -vv
            if not initialize_program (comline) then
               return False;
            end if;
            return extended_version_info;
      end case;
   end execute_no_command;
   

   --------------------------
   --  basic_version_info  --
   --------------------------
   function basic_version_info return Boolean is
   begin
      TIO.Put_Line (progversion);
      return True;
   end basic_version_info;
   
   
   -----------------------
   --  do_status_check  --
   -----------------------
   function do_status_check return Boolean is
   begin
      TIO.Put_Line ("status-check not yet implemented");
      return False;
   end do_status_check;
   
   
   ------------------------------
   --  list_available_comands  --
   ------------------------------
   function list_available_commands return Boolean
   is
      -- start with support for 15 commands (not including unset)
      -- As this is exceeded add an additional row of 5 columns
      type rows is range 1 .. 3;
      type cols is range 1 .. 5;
      type A_column is array (rows) of Command_verb;
      matrix : array (cols) of A_column;
      index : Natural := 0;
   begin
      for x in cols loop
         for y in rows loop
            matrix (x)(y) := cv_unset;
         end loop;
      end loop;
      declare
         row : rows := 1;
         col : cols := 1;
      begin
         for cv in Command_verb'Range loop
            case cv is
               when cv_unset => null;
               when others =>
                  matrix (col)(row) := cv;
                  if row = rows'Last then
                     row := rows'First;
                     col := col + 1;
                  else
                     row := row + 1;
                  end if;
            end case;
         end loop;
      end;
      
      for y in rows loop
         for x in cols loop
            declare
               c : constant String := convert_command_enum_to_label (matrix (x)(y));
            begin
               TIO.Put (pad_right (C, 15));
            end;
         end loop;
         TIO.Put_Line ("");
      end loop;
      return True;
   end list_available_commands;
   
   
   --------------------------
   --  initialize_program  --
   --------------------------
   function initialize_program (comline : Cldata) return Boolean is
   begin
      --  TODO: Implement
      return False;
   end initialize_program;
   
   
   -----------------------------
   --  extended_version_info  --
   -----------------------------
   function extended_version_info return Boolean is
   begin
      TIO.Put_Line ("Version: " & progversion);
      --  TODO: Finish
      --  TIO.Put_Line (Config.Read.config_dump);

      show_repository_info;

      return True;
   end extended_version_info;
   
   
   ----------------------------
   --  show_repository_info  --
   ----------------------------
   procedure show_repository_info is
   begin
      --  TODO: Re-implement
      null;
   end show_repository_info;
   
end Raven.Cmd.Unset;
