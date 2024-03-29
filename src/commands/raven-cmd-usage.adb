--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Strings; use Raven.Strings;
with Archive.Unix;

package body Raven.Cmd.Usage is

   package UNX renames Archive.Unix;

   -----------------------------
   --  precheck_command_line  --
   -----------------------------
   function precheck_command_line (comline : Cldata) return precheck_result
   is
      procedure alert (error_msg : String)
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
      end alert;
   begin
      if comline.parse_error then
         alert (USS (comline.error_message));
         return error_found;
      end if;

      if comline.pending_argument then
         alert ("The last switch requires an argument");
         return error_found;
      end if;

      if not IsBlank (comline.pre_command.install_rootdir) and
        not IsBlank (comline.pre_command.chroot_first)
      then
         alert ("-c and -r are mutually exclusive");
         return error_found;
      end if;

      if not IsBlank (comline.pre_command.install_rootdir) then
         declare
            features : UNX.File_Characteristics :=
              UNX.get_charactistics (USS (comline.pre_command.install_rootdir));
         begin
            case features.ftype is
               when Archive.directory => null;
               when Archive.unsupported =>
                  alert ("rootdir does not exist");
                  return error_found;
               when others =>
                  alert ("rootdir exists, but is not a directory");
                  return error_found;
            end case;
         end;
      end if;

      if not IsBlank (comline.pre_command.chroot_first) then
         declare
            features : UNX.File_Characteristics :=
              UNX.get_charactistics (USS (comline.pre_command.chroot_first));
         begin
            case features.ftype is
               when Archive.directory => null;
               when Archive.unsupported =>
                  alert ("the chroot directory does not exist");
                  return error_found;
               when others =>
                  alert ("chroot exists, but is not a directory");
                  return error_found;
            end case;
         end;
      end if;

      if comline.pre_command.status_check or else
        comline.pre_command.list_commands or else
        comline.pre_command.version_setting /= not_shown
      then
         return action_needed;
      end if;

      if IsBlank (comline.next_argument) then
         alert ("No commands specified");
         return nothing_to_do;
      end if;
      return command_pending;

   end precheck_command_line;


   --------------------------
   --  command_line_valid  --
   --------------------------
   function command_line_valid (comline : Cldata) return Boolean is
   begin
      case comline.command is
         when cv_unset   => return True;  -- already verified
         when cv_alias   => return verb_alias (comline);
         when cv_config  => return verb_config (comline);
         when cv_create  => return verb_create (comline);
         when cv_help    => return verb_help (comline);
         when cv_info    => return verb_info (comline);
         when cv_install => return verb_install (comline);
         when cv_shell   => return True;  -- never shows usage (all args passed though)
         when cv_shlib   => return verb_shlib (comline);
         when cv_which   => return verb_which (comline);
         when cv_version => return verb_version (comline);
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
   --  display_usage_multiline  --
   -------------------------------
   procedure display_usage_multiline (usage_msg : String)
   is
      blanks : constant String (1 .. progname'Length) := (others => ' ');
   begin
      TIO.Put_Line (TIO.Standard_Error, "       " & blanks & " " & usage_msg);
   end display_usage_multiline;


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


   -------------------
   --  verb_config  --
   -------------------
   function verb_config (comline : Cldata) return Boolean
   is
      function alert (error_msg : String) return Boolean
      is
         msg : constant String := "config <name>";
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
   end verb_config;


   -------------------
   --  verb_alias  --
   -------------------
   function verb_alias (comline : Cldata) return Boolean
   is
      function alert (error_msg : String) return Boolean
      is
         msg : constant String := "alias [-ql] [alias]";
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
   end verb_alias;


   -------------------
   --  verb_create  --
   -------------------
   function verb_create (comline : Cldata) return Boolean
   is
      function alert (error_msg : String) return Boolean
      is
         m1 : constant String := "create [-qv] [-o outdir] [-r rootdir] [-p prefix] ";
         m2 : constant String := "       [-w whitelist] [-m metadata] [-t timestamp] pkg-name";
      begin
         display_error (error_msg);
         display_usage (m1, True);
         display_usage (m2, False);
         display_help_suggestion (cv_create);
         return False;
      end alert;
   begin
      if comline.parse_error then
         return alert (USS (comline.error_message));
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
      list_opts : Natural := 0;

      function alert (error_msg : String) return Boolean
      is
         msg1 : constant String := "info <pkg-name>";
         msg2 : constant String := "info -a";
         msg3 : constant String := "info [-ABbMDdefIpqrsNSV] [-L|-l] [-Cgix] <pkg-name>";
         msg4 : constant String := "info [-ABbMDdfIpqRsNSV] [-L|-l|-X] -F <pkg-file>";
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
            if comline.cmd_info.raw_manifest then
               return alert ("--raw manifest only available from pkg-file (-F)");
            end if;
         end if;

         if IsBlank (comline.cmd_info.path_archive_file) then
            if not comline.common_options.all_installed_pkgs and then
              IsBlank (comline.common_options.name_pattern)
            then
               return alert ("Missing <pkg-name>");
            end if;
         else
            if comline.cmd_info.installed then
               return alert ("--exists switch invalid when used with pkg-file (-F)");
            end if;
            if comline.cmd_info.rev_deps then
               return alert ("--required-by switch invalid when used with pkg-file (-F)");
            end if;
         end if;

         if comline.cmd_info.list_files then
            list_opts := list_opts + 1;
         end if;
         if comline.cmd_info.list_digests then
            list_opts := list_opts + 1;
         end if;
         if comline.cmd_info.list_attributes then
            list_opts := list_opts + 1;
         end if;
         if list_opts > 1 then
            return alert
              ("--list-files, --list-digests, and --list-extended are mutually exclusive");
         end if;

         return True;
      end if;
   end verb_info;


   --------------------
   --  verb_install  --
   --------------------
   function verb_install (comline : Cldata) return Boolean
   is
      function alert (error_msg : String) return Boolean
      is
         msg1 : constant String := "install [-AfIMnFqRUy] [-r reponame] [-Cgix] <pkg-name-pattern>";
         msg2 : constant String := "install --[[no|only]-registration| [--file] <path-rvn-archive>";
      begin
         display_error (error_msg);
         display_usage (msg1, True);
         display_usage (msg2, False);
         display_help_suggestion (cv_install);
         return False;
      end alert;

      not_with_file : constant String := " switch is incompatible with --file switch";
   begin
      if comline.parse_error then
         return alert (USS (comline.error_message));
      end if;
      if comline.cmd_install.no_register and then comline.cmd_install.only_register then
         return alert ("--no-registration and --only-registration are mutually exclusive.");
      end if;
      if comline.cmd_install.local_file then
         if comline.cmd_install.fetch_only then
            return alert ("--fetch-only" & not_with_file);
         end if;
         if comline.common_options.no_repo_update then
            return alert ("--no-repo-update" & not_with_file);
         end if;
         if not IsBlank (comline.common_options.repo_name) then
            return alert ("--repository" & not_with_file);
         end if;
         if comline.common_options.case_insensitive or else
           comline.common_options.case_sensitive or else
           comline.common_options.regex or else
           comline.common_options.shell_glob
         then
            return alert ("-Cgix" & not_with_file);
         end if;
         if comline.common_options.multiple_patterns.Is_Empty then
            return alert ("Missing path to rvn archive");
         end if;
         if Natural (comline.common_options.multiple_patterns.Length) > 1 then
            return alert ("Multiple file paths unsupported.  Limit to 1.");
         end if;
         if comline.cmd_install.recursive then
            return alert ("--recursive" & not_with_file);
         end if;
      end if;

      if comline.common_options.multiple_patterns.Is_Empty then
         return alert ("Missing package name pattern");
      end if;

      if comline.common_options.quiet and then
        comline.common_options.dry_run
      then
         return alert ("--dry-run and --quiet are incompatible options.");
      end if;

      return True;
   end verb_install;


   ----------------------------------
   --  alert_command_unrecognized  --
   ----------------------------------
   procedure alert_command_unrecognized (comline : Cldata)
   is
      msg : constant String := "unrecognized command: " & USS (comline.next_argument);
   begin
      display_error (msg);
   end alert_command_unrecognized;



   -------------------
   --  verb_shlib  --
   -------------------
   function verb_shlib (comline : Cldata) return Boolean
   is
      function alert (error_msg : String) return Boolean
      is
         msg : constant String := "shlib [-q] [-P|R] <library>";
      begin
         display_error (error_msg);
         display_usage (msg, True);
         return False;
      end alert;
   begin
      if comline.parse_error then
         return alert (USS (comline.error_message));
      end if;
      if comline.cmd_shlib.provides and then comline.cmd_shlib.requires then
         return alert ("--provides and --requires are mutually exclusive.");
      end if;
      if leads (comline.common_options.name_pattern, "/") then
         return alert ("<library> should be a filename without leading path.");
      end if;
      if IsBlank (comline.common_options.name_pattern) then
         return alert ("<library> is required.");
      end if;

      return True;
   end verb_shlib;


   -------------------
   --  verb_which  --
   -------------------
   function verb_which (comline : Cldata) return Boolean
   is
      function alert (error_msg : String) return Boolean
      is
         msg : constant String := "which [-gmpq] <file|pattern>";
      begin
         display_error (error_msg);
         display_usage (msg, True);
         return False;
      end alert;
   begin
      if comline.parse_error then
         return alert (USS (comline.error_message));
      end if;
      if comline.cmd_which.show_match and not comline.common_options.shell_glob then
         return alert ("-m can only be used with -g");
      end if;
      if IsBlank (comline.common_options.name_pattern) then
         return alert ("<file|pattern> is required.");
      end if;
      return True;
   end verb_which;


   --------------------
   --  verb_version  --
   --------------------
   function verb_version (comline : Cldata) return Boolean
   is
      procedure print_usage
      is
         cegx : constant String := "[-Cegix pattern]";
         msg1 : constant String := "version [-SIR] [qvU] [-l limchar] [-L limchar] " & cegx;
         msg2 : constant String := "        [-r reponame] [-n pkgname]";
         msg3 : constant String := "version -t <version1> <version2>";
         msg4 : constant String := "version -T <pkgname> <pattern>";
      begin
         display_usage (msg1, True);
         display_usage_multiline (msg2);
         display_usage (msg3, False);
         display_usage (msg4, False);
      end print_usage;

      function alert (error_msg : String) return Boolean is
      begin
         display_error (error_msg);
         print_usage;
         display_help_suggestion (cv_version);
         return False;
      end alert;

   begin
      if comline.parse_error then
         return alert (USS (comline.error_message));
      end if;

      case comline.cmd_version.match_char is
         when Character'First => null;
         when '<' | '>' | '=' | '?' | '!' => null;
         when others => return alert ("Illegal character for -l switch");
      end case;

      case comline.cmd_version.not_char is
         when Character'First => null;
         when '<' | '>' | '=' | '?' | '!' => null;
         when others => return alert ("Illegal character for -L switch");
      end case;

      if comline.cmd_version.behavior = test_versions then
         if IsBlank (comline.cmd_version.test1) or else
           IsBlank (comline.cmd_version.test2)
         then
            return alert ("--test-version requires 2 arguments");
         end if;
      end if;

      if comline.cmd_version.behavior = compare_against_pattern then
         if IsBlank (comline.cmd_version.test1) or else
           IsBlank (comline.cmd_version.test2)
         then
            return alert ("--test-pattern requires 2 arguments");
         end if;
      end if;

      return True;
   end verb_version;


end Raven.Cmd.Usage;
