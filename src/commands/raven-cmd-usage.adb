--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Context;
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
         m3 : constant String := "[-4|-6] <command> [<args>]";
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
         when cv_unset      => return True;  -- already verified
         when cv_alias      => return verb_alias (comline);
         when cv_annotate   => return verb_note (comline);
         when cv_autoremove => return verb_autorem (comline);
         when cv_catalog    => return verb_catalog (comline);
         when cv_check      => return verb_check (comline);
         when cv_clean      => return verb_clean (comline);
         when cv_config     => return verb_config (comline);
         when cv_create     => return verb_create (comline);
         when cv_fetch      => return verb_fetch (comline);
         when cv_genrepo    => return verb_genrepo (comline);
         when cv_help       => return verb_help (comline);
         when cv_info       => return verb_info (comline);
         when cv_install    => return verb_install (comline);
         when cv_query      => return verb_query (comline);
         when cv_rquery     => return verb_rquery (comline);
         when cv_remove     => return verb_remove (comline);
         when cv_search     => return verb_search (comline);
         when cv_shell      => return verb_shell (comline);
         when cv_shlib      => return verb_shlib (comline);
         when cv_stats      => return verb_stats (comline);
         when cv_which      => return verb_which (comline);
         when cv_version    => return verb_version (comline);
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


   --------------------
   --  verb_catalog  --
   --------------------
   function verb_catalog (comline : Cldata) return Boolean
   is
      function alert (error_msg : String) return Boolean
      is
         msg : constant String := "catalog [-fq] [-r reponame]";
      begin
         display_error (error_msg);
         display_usage (msg, True);
         return False;
      end alert;
   begin
      if comline.parse_error then
         return alert (USS (comline.error_message));
      end if;
      if not Archive.Unix.user_is_root then
         return alert ("The catalog command is restricted to the superuser.");
      end if;
      return True;
   end verb_catalog;


   ------------------
   --  verb_clean  --
   ------------------
   function verb_clean (comline : Cldata) return Boolean
   is
      function alert (error_msg : String) return Boolean
      is
         msg : constant String := "clean [-anqy]";
      begin
         display_error (error_msg);
         display_usage (msg, True);
         return False;
      end alert;
   begin
      if comline.parse_error then
         return alert (USS (comline.error_message));
      end if;
      if comline.common_options.quiet and then
        comline.common_options.assume_yes and then
        comline.common_options.dry_run and then
        comline.cmd_clean.delete_all
      then
         return alert ("Setting all four options is not permitted, see man page");
      end if;
      if not Archive.Unix.user_is_root then
         return alert ("The clean command is restricted to the superuser.");
      end if;
      return True;
   end verb_clean;


   ------------------
   --  verb_shell  --
   ------------------
   function verb_shell (comline : Cldata) return Boolean
   is
      function alert (error_msg : String) return Boolean
      is
         msg : constant String := "shell [all arguments passed to sqlite]";
      begin
         display_error (error_msg);
         display_usage (msg, True);
         return False;
      end alert;
   begin
      if not Archive.Unix.user_is_root then
         return alert ("The shell command is restricted to the superuser.");
      end if;
      return True;
   end verb_shell;


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
         msg3 : constant String := "info [-ABbMDZdefIpqrsNSV] [-L|-l] [-CE] <pkg-name>";
         msg4 : constant String := "info [-ABbMDZdfIpqRsNSV] [-L|-l|-X] -F <pkg-file>";
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
         if comline.common_options.exact_match and then
           comline.common_options.case_sensitive
         then
            return alert ("--exact-match and --case-sensitive (glob) are incompatible switches");
         end if;

         if comline.cmd_info.installed then
            if not comline.common_options.exact_match then
               return alert ("--exist requires --exact-match to also be set");
            end if;
         end if;

         if not IsBlank (comline.common_options.name_pattern) then
            if comline.common_options.all_installed_pkgs or else
              not IsBlank (comline.cmd_info.path_archive_file)
            then
               return alert ("<pkg-name> not used with -a or -F switch");
            end if;
            if comline.cmd_info.raw_manifest then
               return alert ("--raw manifest only available from pkg-file (-F)");
            end if;
            if comline.cmd_info.list_attributes then
               return alert ("--list-extended only available from pkg-file (-F)");
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
            if comline.common_options.exact_match or else
              comline.common_options.case_sensitive
            then
               return alert ("-CE switches are invalid with pkg-file (-F)");
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
         msg1 : constant String := "install [-AfIMnFqRUy] [-r reponame] [-CE] <pkg-name-pattern>";
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
         if comline.common_options.exact_match or else
           comline.common_options.case_sensitive
         then
            return alert ("-CE" & not_with_file);
         end if;
         if comline.common_options.exact_match and then
           comline.common_options.case_sensitive
         then
            return alert ("--exact-match and --case-sensitive (glob) are incompatible switches");
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

      if not Archive.Unix.user_is_root then
         return alert ("The install command is restricted to the superuser.");
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
      if comline.cmd_which.show_match and not comline.cmd_which.glob_input then
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
      function alert (error_msg : String) return Boolean
      is
         msg1 : constant String := "version [-SIR] [-vU] [-r reponame] [-l flag] [-L flag] "&
                                           "[CE] pattern";
         msg2 : constant String := "version -t <version1> <version2>";
         msg3 : constant String := "version -T <pkgname> <pattern>";
      begin
         display_error (error_msg);
         display_usage (msg1, True);
         display_usage (msg2, False);
         display_usage (msg3, False);
         display_help_suggestion (cv_version);
         return False;
      end alert;

   begin
      if comline.parse_error then
         return alert (USS (comline.error_message));
      end if;

      case comline.cmd_version.match_char is
         when Character'First => null;
         when '<' | '>' | '=' | '?' => null;
         when others => return alert ("Illegal character for -l switch");
      end case;

      case comline.cmd_version.not_char is
         when Character'First => null;
         when '<' | '>' | '=' | '?' => null;
         when others => return alert ("Illegal character for -L switch");
      end case;

      if not IsBlank (comline.common_options.repo_name) then
         case comline.cmd_version.behavior is
            when use_remote_catalog_state => null;
            when others =>
               return alert ("The -r switch can only be used with the --remote switch");
         end case;
      end if;

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

      if comline.common_options.exact_match and then
        comline.common_options.case_sensitive
      then
         return alert ("--exact-match and --case-sensitive (glob) are incompatible switches");
      end if;

      if IsBlank (comline.common_options.name_pattern) then
         if comline.common_options.exact_match then
            return alert ("--exact-match requires a pattern, which is missing");
         elsif comline.common_options.case_sensitive then
            return alert ("--case-sensitive requires a pattern, which is missing");
         end if;
      end if;

      return True;
   end verb_version;


   --------------------
   --  verb_genrepo  --
   --------------------
   function verb_genrepo (comline : Cldata) return Boolean
   is
      function alert (error_msg : String) return Boolean
      is
         msg1 : constant String := "genrepo [-q] [-k private-key] [-p public-key] [-x sign-cmd]";
         msg2 : constant String := "        [-f output-path] <repo-path>";
      begin
         display_error (error_msg);
         display_usage (msg1, True);
         display_usage_multiline (msg2);
         display_help_suggestion (cv_genrepo);
         return False;
      end alert;

      passed_private_key : constant Boolean := not IsBlank (comline.cmd_genrepo.key_private);
      passed_public_key  : constant Boolean := not IsBlank (comline.cmd_genrepo.key_public);
      passed_fingerprint : constant Boolean := not IsBlank (comline.cmd_genrepo.fprint_file);
      passed_ext_command : constant Boolean := not IsBlank (comline.cmd_genrepo.sign_command);
   begin
      if comline.parse_error then
         return alert (USS (comline.error_message));
      end if;

      if passed_ext_command then
         if passed_private_key or else passed_public_key then
            return alert ("--external can not be used with --key or --pubkey");
         end if;
      end if;

      if passed_public_key and then not passed_private_key
      then
         return alert ("--pubkey can not be used without --key");
      end if;

      if not Archive.Unix.user_is_root then
         return alert ("The genrepo command is restricted to the superuser.");
      end if;

       if IsBlank (comline.common_options.name_pattern) then
         return alert ("<repo-path> is required.");
      else
         declare
            features : UNX.File_Characteristics :=
              UNX.get_charactistics (USS (comline.common_options.name_pattern));
         begin
            case features.ftype is
               when Archive.directory => null;
               when Archive.unsupported =>
                  return alert ("<repo-path> does not exist");
               when others =>
                  return alert ("<repo-path> exists, but is not a directory");
            end case;
         end;
      end if;

      if passed_public_key then
         declare
            features : UNX.File_Characteristics :=
              UNX.get_charactistics (USS (comline.cmd_genrepo.key_public));
         begin
            case features.ftype is
               when Archive.regular => null;
               when Archive.unsupported =>
                  return alert ("<public-key> file does not exist");
               when others =>
                  return alert ("<public-key> exists but is not a regular file");
            end case;
         end;
      end if;

      if passed_private_key then
         declare
            features : UNX.File_Characteristics :=
              UNX.get_charactistics (USS (comline.cmd_genrepo.key_private));
         begin
            case features.ftype is
               when Archive.regular => null;
               when Archive.unsupported =>
                  return alert ("<private-key> file does not exist");
               when others =>
                  return alert ("<private-key> exists but is not a regular file");
            end case;
         end;
      end if;

      if passed_fingerprint then
         if not passed_ext_command and not passed_public_key then
            return alert ("--fingerprint requires --external or --pubkey option to be set.");
         end if;
         --  verify parent directory exists
         if contains (comline.cmd_genrepo.fprint_file, "/") then
            declare
               features   : UNX.File_Characteristics;
               parent_dir : constant String := head (USS (comline.cmd_genrepo.fprint_file), "/");
            begin
               features := UNX.get_charactistics (parent_dir);
               case features.ftype is
                  when Archive.directory => null;
                  when Archive.unsupported =>
                     return alert ("<output-file> parent directory does not exist");
                  when others =>
                     return alert ("<output-file> parent exists but is not a directory");
               end case;
            end;
         end if;
      end if;

      return True;

   end verb_genrepo;


   ------------------
   --  verb_query  --
   ------------------
   function verb_query (comline : CLdata) return Boolean
   is
      function alert (error_msg : String) return Boolean
      is
         msg1 : constant String := "query [-a] <query-format>";
         msg2 : constant String := "query [-CE] [-e <eval-condition>] <query-format> <pattern>";
      begin
         display_error (error_msg);
         display_usage (msg1, True);
         display_usage (msg2, False);
         display_help_suggestion (cv_query);
         return False;
      end alert;
   begin
      if comline.common_options.exact_match and then
        comline.common_options.case_sensitive
      then
         return alert ("--exact-match and --case-sensitive (glob) are incompatible switches");
      end if;

      if IsBlank (comline.cmd_query.query_format) then
         return alert ("<query-format> cannot be undefined");
      end if;

      if comline.common_options.all_installed_pkgs then
        if not IsBlank (comline.common_options.name_pattern) then
            return alert ("--all is incompatible with <pattern>");
         end if;
      else
         if IsBlank (comline.common_options.name_pattern) then
            return alert ("<pattern> cannot be undefined without --all switch set");
         end if;
      end if;

      return True;

   end verb_query;


   -------------------
   --  verb_rquery  --
   -------------------
   function verb_rquery (comline : CLdata) return Boolean
   is
      function alert (error_msg : String) return Boolean
      is
         msg1 : constant String := "query [-a] <query-format>";
         msg2 : constant String := "query [-U] [-CE] [-r reponame] [-e <eval-condition>] ";
         msg3 : constant String := "      <query-format> <pattern>";
      begin
         display_error (error_msg);
         display_usage (msg1, True);
         display_usage (msg2, False);
         display_usage_multiline (msg3);
         display_help_suggestion (cv_rquery);
         return False;
      end alert;
   begin
      if comline.common_options.exact_match and then
        comline.common_options.case_sensitive
      then
         return alert ("--exact-match and --case-sensitive (glob) are incompatible switches");
      end if;

      if IsBlank (comline.cmd_rquery.query_format) then
         return alert ("<query-format> cannot be undefined");
      end if;

      if comline.common_options.all_installed_pkgs then
        if not IsBlank (comline.common_options.name_pattern) then
            return alert ("--all is incompatible with <pattern>");
         end if;
      else
         if IsBlank (comline.common_options.name_pattern) then
            return alert ("<pattern> cannot be undefined without --all switch set");
         end if;
      end if;

      return True;
   end verb_rquery;


   ------------------
   --  verb_stats  --
   ------------------
   function verb_stats (comline : CLdata) return Boolean
   is
      function alert (error_msg : String) return Boolean
      is
         msg1 : constant String := "stats [-U] [-lc] [-r reponame]";
      begin
         display_error (error_msg);
         display_usage (msg1, True);
         display_help_suggestion (cv_stats);
         return False;
      end alert;
   begin
      if comline.cmd_stats.catalog_only and then
        comline.cmd_stats.local_only
      then
         return alert ("--local and --catalog are mutually exclusive.");
      end if;

      return True;
   end verb_stats;


   -------------------
   --  verb_search  --
   -------------------
   function verb_search (comline : CLdata) return Boolean
   is
      mod_count : Natural := 0;

      function alert (error_msg : String) return Boolean
      is
         msg1 : constant String := "search [-U] [-r reponame] [-CEg] [-cdnt] [-Q query-modifier]*";
         msg2 : constant String := "       <search pattern>";
      begin
         display_error (error_msg);
         display_usage (msg1, True);
         display_usage_multiline (msg2);
         display_help_suggestion (cv_search);
         return False;
      end alert;
   begin
      if comline.parse_error then
         return alert (USS (comline.error_message));
      end if;

      if comline.common_options.exact_match then
         mod_count := mod_count + 1;
      end if;

      if comline.common_options.case_sensitive then
         mod_count := mod_count + 1;
      end if;

      if comline.cmd_search.glob_input then
         mod_count := mod_count + 1;
      end if;

      if mod_count > 1 then
         return alert ("--exact-match, --case-sensitive, and --glob are incompatible switches");
      end if;

      if Context.reveal_case_sensitive then
         if mod_count = 1 then
            if not comline.common_options.case_sensitive then
               if comline.common_options.exact_match then
                  return alert ("--exact-match is incompatible with CASE_SENSITIVE_MATCH=true");
               end if;
               if comline.cmd_search.glob_input then
                  return alert ("--glob is incompatible with CASE_SENSITIVE_MATCH=true");
               end if;
            end if;
         end if;
      end if;

      if isBlank (comline.cmd_search.spattern) then
         return alert ("The required <search-pattern> argument is missing.");
      end if;

      return True;

   end verb_search;


   ------------------
   --  verb_fetch  --
   ------------------
   function verb_fetch (comline : Cldata) return Boolean
   is
      function alert (error_msg : String) return Boolean
      is
         msg1 : constant String := "fetch [-r reponame] [-qUy] [-o destdir] -a";
         msg2 : constant String := "fetch [-r reponame] [-qUy] [-o destdir] -u";
         msg3 : constant String := "fetch [-r reponame] [-qUy] [-o destdir] [-d] [-CE] " &
                                   "pattern [...]";
      begin
         display_error (error_msg);
         display_usage (msg1, True);
         display_usage (msg2, False);
         display_usage (msg3, False);
         display_help_suggestion (cv_fetch);
         return False;
      end alert;

      dua_count : Natural := 0;
   begin
      if comline.common_options.exact_match and then
        comline.common_options.case_sensitive
      then
         return alert ("--exact-match and --case-sensitive (glob) are incompatible switches");
      end if;

      if comline.common_options.all_installed_pkgs then
         dua_count := dua_count + 1;
         if comline.common_options.exact_match or else
           comline.common_options.case_sensitive
         then
            return alert ("--all not compatible with -C or -E switches");
         end if;
      end if;

      if comline.cmd_fetch.avail_updates then
         dua_count := dua_count + 1;
         if comline.common_options.exact_match or else
           comline.common_options.case_sensitive
         then
            return alert ("--available-updates not compatible with -C or -E switches");
         end if;
      end if;

      if comline.cmd_fetch.depends_also then
         dua_count := dua_count + 1;
      end if;

      if dua_count > 1 then
         return alert ("Select only one of --all, --dependencies, and --available-updates");
      end if;

      if not IsBlank (comline.cmd_fetch.destination) then
         declare
            features : Archive.Unix.File_Characteristics;
         begin
            features := Archive.Unix.get_charactistics (USS (comline.cmd_fetch.destination));
            case features.ftype is
               when Archive.directory => null;
               when Archive.unsupported =>
                  return alert ("--output target does not exist (directory expected)");
               when others =>
                  return alert ("--output target exists, but is not a directory.");
            end case;
         end;
      end if;

      if comline.cmd_fetch.name_patterns.Is_Empty then
         if not comline.common_options.all_installed_pkgs and
           not comline.cmd_fetch.avail_updates
         then
            return alert ("One or more file name patterns is required with these switches.");
         end if;
      end if;

      return True;

   end verb_fetch;


   -------------------
   --  verb_remove  --
   -------------------
   function verb_remove  (comline : Cldata) return Boolean
   is
      function alert (error_msg : String) return Boolean
      is
         msg1 : constant String := "remove [-fInqsy] [-CE] pattern [...]";
         msg2 : constant String := "remove [-fInqsy] -a";
      begin
         display_error (error_msg);
         display_usage (msg1, True);
         display_usage (msg2, False);
         display_help_suggestion (cv_remove);
         return False;
      end alert;
   begin
       if comline.common_options.exact_match and then
        comline.common_options.case_sensitive
      then
         return alert ("--exact-match and --case-sensitive (glob) are incompatible switches");
      end if;

      if comline.common_options.all_installed_pkgs then
         if comline.common_options.exact_match or else
           comline.common_options.case_sensitive
         then
            return alert ("--all not compatible with -C or -E switches");
         end if;
      end if;

      if comline.cmd_remove.name_patterns.Is_Empty and then
        not comline.common_options.all_installed_pkgs
      then
         return alert ("One or more file name patterns unless --all specified");
      end if;

      if not Archive.Unix.user_is_root then
         return alert ("The remove command is restricted to the superuser.");
      end if;

      if comline.common_options.dry_run and then
        comline.common_options.quiet
      then
         return alert ("--dry-run and --quiet are incompatible switches");
      end if;

      return True;

   end verb_remove;


   --------------------
   --  verb_autorem  --
   --------------------
   function verb_autorem (comline : Cldata) return Boolean
   is
      function alert (error_msg : String) return Boolean
      is
         msg1 : constant String := "autoremove [-Inqsy]";
      begin
         display_error (error_msg);
         display_usage (msg1, True);
         display_help_suggestion (cv_autoremove);
         return False;
      end alert;
   begin
      if not Archive.Unix.user_is_root then
         return alert ("The autoremove command is restricted to the superuser.");
      end if;

      if comline.common_options.dry_run and then
        comline.common_options.quiet
      then
         return alert ("--dry-run and --quiet are incompatible switches");
      end if;

      return True;
   end verb_autorem;


   ------------------
   --  verb_check  --
   ------------------
   function verb_check (comline : Cldata) return Boolean
   is
      function alert (error_msg : String) return Boolean
      is
         msg1 : constant String := "check [-d|-f] [-q|-v]";
      begin
         display_error (error_msg);
         display_usage (msg1, True);
         display_help_suggestion (cv_check);
         return False;
      end alert;
   begin
      if comline.cmd_check.only_depends and then
        comline.cmd_check.only_files
      then
         return alert ("--depends-only and --files-only are incompatible switches");
      end if;

      if comline.common_options.verbose and then
        comline.common_options.quiet
      then
         return alert ("--quiet and --verbose are incompatible switches");
      end if;

      return True;
   end verb_check;


   -----------------
   --  verb_note  --
   -----------------
   function verb_note (comline : Cldata) return Boolean
   is
      mod_count : Natural := 0;
      op_count  : Natural := 0;

      function alert (error_msg : String) return Boolean
      is
         patt : constant String := "[-CEg] pattern";
         msg1 : constant String := "annotate -s -t <tag> -n <note> [-qy] " & patt;
         msg2 : constant String := "annotate -d -t <tag> [-qy] " & patt;
         msg3 : constant String := "annotate -f -t <tag> " & patt;
      begin
         display_error (error_msg);
         display_usage (msg1, True);
         display_usage (msg2, False);
         display_usage (msg3, False);
         display_help_suggestion (cv_annotate);
         return False;
      end alert;
   begin
      if comline.common_options.exact_match then
         mod_count := mod_count + 1;
      end if;

      if comline.common_options.case_sensitive then
         mod_count := mod_count + 1;
      end if;

      if comline.cmd_annotate.glob_input then
         mod_count := mod_count + 1;
      end if;

      if mod_count > 1 then
         return alert ("--exact-match, --case-sensitive, and --glob are incompatible switches");
      end if;

      if comline.cmd_annotate.operation_delete then
         op_count := op_count + 1;
      end if;

      if comline.cmd_annotate.operation_find then
         op_count := op_count + 1;
      end if;

      if comline.cmd_annotate.operation_set then
         op_count := op_count + 1;
      end if;

      if op_count > 1 then
         return alert ("--set, --delete, and --find are incompatible switches");
      end if;

      if op_count = 0 then
         return alert ("--set, --delete, or --find must be specified");
      end if;

      if not Archive.Unix.user_is_root then
         if comline.cmd_annotate.operation_set or else
           comline.cmd_annotate.operation_delete
         then
            return alert ("This operation is restricted to the superuser.");
         end if;
      end if;

      if IsBlank (comline.cmd_annotate.tag) then
         return alert ("The --tag is required.");
      end if;

      if comline.cmd_annotate.operation_set then
         if IsBlank (comline.cmd_annotate.note) then
            return alert ("The --note is required for the --set operation.");
         end if;
         if IsBlank (comline.common_options.name_pattern) then
            return alert ("<pattern> is required for the set operation");
         end if;
      end if;

      return True;
   end verb_note;

end Raven.Cmd.Usage;
