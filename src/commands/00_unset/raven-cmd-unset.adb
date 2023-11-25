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
      type cols is range 1 .. 5;

      numwords    : constant Natural := Command_verb'Range_Length - 1;
      minlength   : constant Natural := numwords / cols'Range_Length;

      col_length  : constant array (cols) of Natural := (others => minlength);
      print_order : array (1 .. numwords) of Command_verb;

      column : cols := cols'First;
   begin
      --  Loop will not execute when total commands - 1 is divisible by 5
      --  Currently at this point, uncomment when new commands are added
--        for N in 1 .. cols_minus1 loop
--           col_length (cols (N)) := minlength + 1;
--        end loop;

      declare
         po_index : Natural := 1;  --  Zero-indexed, but we want to skip the first command
      begin
         for N in cols'Range loop
            declare
               index : Positive := Positive (N);
            begin
               for Q in 1 .. col_length (N) loop
                  print_order (index) := Command_verb'Val (po_index);
                  po_index := po_index + 1;
                  index := index + Positive (cols'Last);
               end loop;
            end;
         end loop;
      end;

      for cindex in print_order'Range loop
         declare
            command : constant Command_verb := print_order (cindex);
            C : constant String := convert_command_enum_to_label (command);
         begin
            case command is
               when cv_unset => null;
               when others =>
                  TIO.Put (pad_right (C, 15));
                  if column = cols'Last then
                     column := cols'First;
                     TIO.Put_Line ("");
                  else
                     column := column + 1;
                  end if;
            end case;
         end;
      end loop;
      if column > cols'First then
         TIO.Put_Line ("");
      end if;
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
