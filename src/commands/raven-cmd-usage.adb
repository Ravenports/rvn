--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Raven.Strings; use Raven.Strings;

package body Raven.Cmd.Usage is

   --------------------------
   --  command_line_valid  --
   --------------------------
   function command_line_valid (comline : Cldata) return Boolean is
   begin
      case comline.command is
         when cv_unset  => return no_command_verb (comline);
         when cv_create => return verb_create (comline);
         when cv_help   => return verb_help (comline);
         when cv_info   => return verb_info (comline);
      end case;
   end command_line_valid;
   
   
   ---------------------
   --  display_error  --
   ---------------------
   procedure display_error (error_msg : String) is
   begin
      if error_msg /= "" then
         TIO.Put_Line (TIO.Standard_Error, progname & ": " & error_msg);
      end if;
   end display_error;
   
   
   ---------------------
   --  display_usage  --
   ---------------------
   procedure display_usage (usage_msg : String; first_line : Boolean)
   is
      tray : String := "Usage: " & progname & " ";
   begin
      if not first_line then
         tray := (others => ' ');
      end if;
      TIO.Put_Line (TIO.Standard_Error, tray & usage_msg);
   end display_usage;
   
   
   -------------------------------
   --  display_help_suggestion  --
   -------------------------------
   procedure display_help_suggestion (command : Command_verb)
   is
      main_msg : constant String :=
        "For more information on available commands and options see "
        & SQ (progname & " help") & ".";
   begin
      insert_carriage_return;
      case command is
         when cv_unset => TIO.Put_Line (TIO.Standard_Error, main_msg);
         when others   => TIO.Put_Line
              (TIO.Standard_Error,
               "For more information see " &
                 SQ (progname & " help " & convert_command_enum_to_label (command)) & ".");
      end case;
   end display_help_suggestion;
   
   
   ------------------------------
   --  insert_carriage_return  --
   ------------------------------
   procedure insert_carriage_return is
   begin
      TIO.Put_Line (TIO.Standard_Error, "");
   end insert_carriage_return;
   
   
   -----------------------
   --  no_command_verb  --
   -----------------------
   function no_command_verb (comline : Cldata) return Boolean
   is
      function alert (error_msg : String) return Boolean;
      function alert (error_msg : String) return Boolean
      is
         m1 : constant String := "[-v] [-d] [-l] [--status-check] [-c <chroot path>|-r <rootdir>]";
         m2 : constant String := "[-C <configuration file>] [-R <repo config dir>] [-o var=value]";
         m3 : constant String := "<command> [<args>]";
      begin
         display_error (error_msg);
         display_usage (m1, True);
         display_usage (m2, False);
         display_usage (m3, False);
         display_help_suggestion (cv_unset);
         return False;
      end alert;
   begin
      if comline.parse_error then
         return alert (USS (comline.error_message));
      end if;
      
      --  check if no arguments given
      if comline.unset_version = not_shown and then       
        not comline.unset_list_cmd and then
        not comline.unset_status_check and then
        comline.global_debug = A_Debug_Level'First and then
        IsBlank (comline.global_chroot) and then
        IsBlank (comline.global_config_file) and then
        IsBlank (comline.global_repo_config_dir) and then
        IsBlank (comline.global_root_dir) and then
        IsBlank (comline.global_options)
      then
         return alert ("Not enough arguments");
      end if;

      --  Only three switches are used without a command verb
      if comline.unset_version = not_shown and then 
        not comline.unset_list_cmd and then
        not comline.unset_status_check
      then
         return alert ("No commands specified");
      end if;

      return True;
   end no_command_verb;

   
   -------------------
   --  verb_create  --
   -------------------
   function verb_create (comline : Cldata) return Boolean
   is
      function alert (error_msg : String) return Boolean;
      function alert (error_msg : String) return Boolean
      is
         msg : constant String := "create [-qv] [-o outdir] [-r rootdir] [-w whitelist] " &
           "[-m metadata] [-t timestamp] pkg-name";
      begin
         display_error (error_msg);
         display_usage (msg, True);
         display_help_suggestion (cv_create);
         return False;
      end alert;
   begin
      if comline.parse_error then
         return alert (USS (comline.error_message));
      end if;
      
      if IsBlank (comline.common_options.name_pattern) then
         return alert ("The pkg-name information is required.");
      end if;
      
      if not IsBlank (comline.cmd_create.timestamp) then
         if not IsNumeric (USS (comline.cmd_create.timestamp)) then
            return alert ("The timestamp value is not numeric.");
         end if;
      end if;   
      
      return True;   
   end verb_create;
   
   
   -----------------
   --  verb_help  --
   -----------------
   function verb_help (comline : Cldata) return Boolean
   is
      function alert (error_msg : String) return Boolean;
      function alert (error_msg : String) return Boolean
      is
         msg : constant String := "help <command>";
      begin
         display_error (error_msg);
         display_usage (msg, True);
         return False;
      end alert;
   begin
      if comline.parse_error then
         return alert (USS (comline.error_message));
      end if;
      return True;
   end verb_help;
   
   
   -----------------
   --  verb_info  --
   -----------------
   function verb_info (comline : Cldata) return Boolean
   is
      function alert (error_msg : String) return Boolean;
      function alert (error_msg : String) return Boolean
      is
         msg1 : constant String := "info <pkg-name>";
         msg2 : constant String := "info -a";
         msg3 : constant String := "info [-ABbMDdefIlpqRrsNSV] [-Cgix] <pkg-name>";
         msg4 : constant String := "info [-ABbMDdfIlpqRrsNSV] -F <pkg-file>";
      begin
         display_error (error_msg);
         display_usage (msg1, True);
         display_usage (msg2, False);
         display_usage (msg3, False);
         display_usage (msg4, False);
         display_help_suggestion (cv_info);
         return False;
      end alert;
   begin
      if comline.parse_error then
         return alert (USS (comline.error_message));
      else
         if not IsBlank (comline.common_options.name_pattern) then
            if comline.common_options.all_installed_pkgs or else 
              not IsBlank (comline.cmd_info.path_archive_file) 
            then
               return alert ("<pkg-name> not used with -a or -F switch");
            end if;
         end if;

         if not comline.common_options.all_installed_pkgs and then
           IsBlank (comline.common_options.name_pattern)
         then
            return alert ("Missing <pkg-name>");
         end if;

         return True;
      end if;
   end verb_info;
   
end Raven.Cmd.Usage;
