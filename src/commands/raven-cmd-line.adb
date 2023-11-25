--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Command_Line;
with Ada.Characters.Latin_1;
with Raven.Strings; use Raven.Strings;

package body Raven.Cmd.Line is

   package CLI renames Ada.Command_Line;
   package LAT renames Ada.Characters.Latin_1;


   --------------------------
   --  parse_command_line  --
   --------------------------
   function parse_command_line return Cldata
   is
      procedure translate_switch (position : string_crate.Cursor);
      procedure set_error (error_msg : String);
      procedure handle_trailing_pkgname (datum : String; datumtxt : Text);
      function aCgix (datum : String; datumtxt : Text; use_all : Boolean := True) return Boolean;
      procedure check_create_incompatibilities;
      procedure check_implied_info_all;

      expanded_args : string_crate.Vector;
      result        : Cldata;
      last_cmd      : Clswitch := nothing_pending;

      sws_all    : constant String := "-a";
      swl_all    : constant String := "--all";
      sws_case   : constant String := "-C";
      swl_case   : constant String := "--case-sensitive";
      sws_icase  : constant String := "-i";
      swl_icase  : constant String := "--case-insensitive";
      sws_glob   : constant String := "-g";
      swl_glob   : constant String := "--glob";
      sws_regex  : constant String := "-x";
      swl_regex  : constant String := "--regex";

      -----------------
      --  set_error  --
      -----------------
      procedure set_error (error_msg : String) is
      begin
         --  Don't overwrite previous errors
         if not result.parse_error then
            result.parse_error := True;
            result.error_message := SUS (error_msg);
         end if;
      end set_error;

      -------------------------------
      --  handle_trailing_pkgname  --
      -------------------------------
      procedure handle_trailing_pkgname (datum : String; datumtxt : Text)
      is
         hyphen : constant Character := '-';
      begin
         if datum (datum'First) = hyphen then
            set_error ("Unexpected switch: " & datum);
         else
            if result.common_options.all_installed_pkgs then
               set_error ("Unexpected file pattern with --all option: " & datum);
            elsif not IsBlank (result.common_options.name_pattern) then
               set_error ("Attempt to redefine file name pattern from "
                          & SQ (USS (result.common_options.name_pattern)) & " to " & DQ (datum));
            else
               result.common_options.name_pattern := datumtxt;
            end if;
         end if;
      end handle_trailing_pkgname;

      -------------
      --  aCgix  --
      -------------
      function aCgix (datum : String; datumtxt : Text; use_all : Boolean := True) return Boolean
      is
         pragma Unreferenced (datumtxt);
      begin
         if use_all and then (datum = sws_all or else datum = swl_all) then
            result.common_options.all_installed_pkgs := True;
         elsif datum = sws_case or else datum = swl_case then
            result.common_options.case_sensitive := True;
         elsif datum = sws_icase or else datum = swl_icase then
            result.common_options.case_insensitive := True;
         elsif datum = sws_glob or else datum = swl_glob then
            result.common_options.shell_glob := True;
         elsif datum = sws_regex or else datum = swl_regex then
            result.common_options.regex := True;
         else
            return False;
         end if;
         return True;
      end aCgix;


      --------------------------------------
      --  check_create_incompatibilities  --
      --------------------------------------
      procedure check_create_incompatibilities
      is
         --  This is kind of a legacy check for pkg users.  Probably not really necessary.
      begin
         if result.command = cv_create then
            if result.common_options.all_installed_pkgs or else
              result.common_options.shell_glob or else
              result.common_options.regex
            then
               set_error ("Switches -a, -g, -x invalid - create requires descrete filename");
            end if;
         end if;
      end check_create_incompatibilities;


      ------------------------------
      --  check_implied_info_all  --
      ------------------------------
      procedure check_implied_info_all is
      begin
         --  These command imply -a
         --  rvn info
         --  any rvn info missing the pkg-name / pattern argument
         if result.command = cv_info then
            if not result.common_options.all_installed_pkgs then
               if IsBlank (result.cmd_info.path_archive_file) then
                  result.common_options.all_installed_pkgs := True;
               end if;
            end if;
         end if;
      end check_implied_info_all;


      ------------------------
      --  translate_switch  --
      ------------------------
      procedure translate_switch (position : string_crate.Cursor)
      is
         datumtxt : Text renames string_crate.Element (position);
         datum : constant String := USS (datumtxt);

         sws_quiet  : constant String := "-q";
         swl_quiet  : constant String := "--quiet";
         sws_dryrun : constant String := "-n";
         swl_dryrun : constant String := "--dry-run";
         sws_yes    : constant String := "-y";
         swl_yes    : constant String := "--yes";
         sws_verb   : constant String := "-v";
         swl_verb   : constant String := "--verbose";
         sws_nocat  : constant String := "-U";
         swl_nocat  : constant String := "--no-repo-update";
         sws_repo   : constant String := "-r";
         swl_repo   : constant String := "--repository";
         sws_help   : constant String := "-h";
         swl_help   : constant String := "--help";
         AME        : constant String := " switches are mutually exclusive.";
         error_rec  : constant String := "Unrecognized option: ";
         error_exp  : constant String := "Unexpected argument: ";
         error_chk  : constant String := "Attempt to redefine check action: ";
         error_ann  : constant String := "Attempt to redefine annotation action: ";
         error_PR   : constant String := "The --provides and --requires" & AME;
         error_MON  : constant String := "The --match-origin and --match-name" & AME;
         error_like : constant String := "The --like and --not-like" & AME;
         hyphen     : constant Character := '-';

      begin
         if result.parse_error then
            return;
         end if;

         if last_cmd = nothing_pending then
            case result.command is
               when cv_unset =>
                  --  global options
                  if datum = "-d" or else datum = "--debug"
                  then
                     if result.global_debug < A_Debug_Level'Last then
                        result.global_debug := result.global_debug + 1;
                     end if;
                  elsif datum = "-v" or else datum = "--version"
                  then
                     case result.unset_version is
                        when not_shown          => result.unset_version := just_version;
                        when just_version       => result.unset_version := dump_configuration;
                        when dump_configuration => null;
                     end case;
                  elsif datum = "-l" or else datum = "--list"
                  then
                     result.unset_list_cmd := True;
                  elsif datum = "--status-check"
                  then
                     result.unset_status_check := True;
                  elsif datum = "-c" or else datum = "--chroot"
                  then
                     last_cmd := global_chroot;
                  elsif datum = "-C" or else datum = "--config"
                  then
                     last_cmd := global_config;
                  elsif datum = "-R" or else datum = "--repo-conf-dir"
                  then
                     last_cmd := global_repoconfdir;
                  elsif datum = "-r" or else datum = "--rootdir"
                  then
                     last_cmd := global_rootdir;
                  elsif datum = "-o" or else datum = "--option"
                  then
                     last_cmd := global_option;
                  else
                     --  This far means either we hit a secondary command or
                     --  we've got an unrecognized option
                     result.command := get_command (datum);
                     if result.command = cv_unset then
                        set_error (error_rec & datum);
                     elsif result.command = cv_help then
                        last_cmd := help;
                     end if;

                  end if;

               when cv_create =>
                  if datum = sws_quiet or else datum = swl_quiet then
                     result.common_options.quiet := True;
                  elsif datum = sws_verb or else datum = swl_verb then
                     result.common_options.verbose := True;
                  elsif datum = "-r" or else datum = "--root-dir" then
                     last_cmd := global_rootdir;
                  elsif datum = "-m" or else datum = "--metadata" then
                     last_cmd := create_metadata;
                  elsif datum = "-w" or else datum = "--whitelist" then
                     last_cmd := create_whitelist;
                  elsif datum = "-o" or else datum = "--out-dir" then
                     last_cmd := create_outdir;
                  elsif datum = "-t" or else datum = "--timestamp" then
                     last_cmd := create_timestamp;
                  else
                     handle_trailing_pkgname (datum, datumtxt);
                  end if;

               when cv_help =>
                  if result.help_command = cv_unset and then
                    result.help_command2 = cv2_unset
                  then
                     last_cmd := help;
                  else
                     set_error ("The help command only takes one argument");
                  end if;

               when cv_info =>
                  if datum = sws_quiet or else datum = swl_quiet then
                     result.common_options.quiet := True;
                  elsif aCgix (datum, datumtxt) then
                     null;
                  elsif datum = "-A" or else datum = "--annotations" then
                     result.cmd_info.annotations := True;
                  elsif datum = "-B" or else datum = "--required-shlibs" then
                     result.cmd_info.shlibs_used := True;
                  elsif datum = "-b" or else datum = "--provided-shlibs" then
                     result.cmd_info.shlibs_provided := True;
                  elsif datum = "-M" or else datum = "--pkg-message" then
                     result.cmd_info.install_message := True;
                  elsif datum = "-D" or else datum = "--description" then
                     result.cmd_info.description := True;
                  elsif datum = "-d" or else datum = "--dependencies" then
                     result.cmd_info.dependencies := True;
                  elsif datum = "-e" or else datum = "--exists" then
                     result.cmd_info.installed := True;
                  elsif datum = "-f" or else datum = "--full" then
                     result.cmd_info.full_information := True;
                  elsif datum = "-I" or else datum = "--comment" then
                     result.cmd_info.comment := True;
                  elsif datum = "-l" or else datum = "--list-files" then
                     result.cmd_info.list_files := True;
                  elsif datum = "-p" or else datum = "--prefix" then
                     result.cmd_info.install_prefix := True;
                  elsif datum = "-R" or else datum = "--raw" then
                     result.cmd_info.raw_manifest := True;
                  elsif datum = "-r" or else datum = "--required-by" then
                     result.cmd_info.rev_deps := True;
                  elsif datum = "-s" or else datum = "--size" then
                     result.cmd_info.total_size := True;
                  elsif datum = "-N" or else datum = "--namebase" then
                     result.cmd_info.namebase := True;
                  elsif datum = "-S" or else datum = "--subpackage" then
                     result.cmd_info.subpackage := True;
                  elsif datum = "-V" or else datum = "--variant" then
                     result.cmd_info.variant := True;
                  elsif datum = "-F" or else datum = "--file" then
                     last_cmd := info_archive_file;
                  else
                     handle_trailing_pkgname (datum, datumtxt);
                     if not IsBlank (result.cmd_info.path_archive_file) then
                        set_error ("Use of -F switch with pkg-name is not permitted.");
                     end if;
                  end if;

            end case;
         else
            --  insert second part of last seen command
            case last_cmd is
               when nothing_pending    => null;   --  impossible
               when global_chroot      => result.global_chroot              := datumtxt;
               when global_config      => result.global_config_file         := datumtxt;
               when global_repoconfdir => result.global_repo_config_dir     := datumtxt;
               when global_rootdir     => result.global_root_dir            := datumtxt;
               when create_metadata    => result.cmd_create.metadata_file   := datumtxt;
               when create_whitelist   => result.cmd_create.whitelist_file  := datumtxt;
               when create_outdir      => result.cmd_create.output_dir      := datumtxt;
               when create_timestamp   => result.cmd_create.timestamp       := datumtxt;
               when info_archive_file  => result.cmd_info.path_archive_file := datumtxt;
               when global_option =>
                  if IsBlank (result.global_options) then
                     result.global_options := datumtxt;
                  else
                     SU.Append (result.global_options, LAT.Vertical_Line & datum);
                  end if;
               when help =>
                  result.help_command := get_command (datum);
                  if result.help_command = cv_unset then
                     if datum = progname then
                        result.help_command2 := cv2_main;
                     elsif datum = progname & ".conf" then
                        result.help_command2 := cv2_main_conf;
                     elsif datum = "repository" then
                        result.help_command2 := cv2_repository;
                     else
                        set_error (SQ (datum) & " is not a recognized command");
                     end if;
                  end if;
            end case;
            last_cmd := nothing_pending;
         end if;
      end translate_switch;
   begin
      expand_command_line (expanded_args);
      expanded_args.Iterate (translate_switch'Access);

      --  TODO: when annotated implemented:
      --  check_annotate_stdin;

      --  TODO: when version re-implemented:
      --  check_version_stdin;

      --  TODO: when rquery implemented:
      --  check_implied_rquery_all;

      --  TODO: when query implemented:
      --  check_implied_query_all;

      --  TODO:  check_stats_default ??

      check_create_incompatibilities;
      check_implied_info_all;

      return result;
   end parse_command_line;


   ---------------------------
   --  expand_command_line  --
   ---------------------------
   procedure expand_command_line (expanded_args : in out string_crate.Vector) is
   begin
      for Arg in 1 .. CLI.Argument_Count loop
         declare
            datum      : constant String := CLI.Argument (Arg);
            save_as_is : Boolean := False;
         begin
            if datum'Length = 1 then
               --  Too small to be any kind of switch, just save it
               --  It also could be a hyphen to indicate stdin output
               save_as_is := True;
            elsif datum (datum'First) = '-' then
               if datum (datum'First + 1) = '-' then
                  if datum'Length = 2 then
                     --  illegal "--" value, just save
                     save_as_is := True;
                  else
                     --  full switch, check for NV
                     if contains (datum, "=") then
                        --  Save both sides separately
                        expanded_args.Append (SUS (part_1 (datum, "=")));
                        expanded_args.Append (SUS (part_2 (datum, "=")));
                     else
                        --  not in NV format, just save it
                        save_as_is := True;
                     end if;
                  end if;
               else
                  --  more or more short switches concatenated, expand
                  for ch in datum'First + 1 .. datum'Last loop
                     expanded_args.Append (SUS ('-' & datum (ch)));
                  end loop;
               end if;
            else
               --  No switch prefix, save without expanding
               save_as_is := True;
            end if;
            if save_as_is then
               expanded_args.Append (SUS (datum));
            end if;
         end;
      end loop;
   end expand_command_line;


   -------------------
   --  get_command  --
   -------------------
   function get_command (component : String) return Command_verb
   is
      total_keywords : constant Positive := Command_verb'Pos (Command_verb'Last) + 1;

      subtype keyword_string is String (1 .. 10);

      type keyword_pair is
         record
            keyword : keyword_string;
            keytype : Command_verb;
         end record;

      --  Keep in alphabetical order (critical!)
      all_keywords : constant array (1 .. total_keywords) of keyword_pair :=
        (
         ("NOTFOUND  ", cv_unset),
         ("create    ", cv_create),
         ("help      ", cv_help),
         ("info      ", cv_info)
         --  ("add       ", cv_add),
         --  ("alias     ", cv_alias),
         --  ("annotate  ", cv_annotate),
         --  ("autoremove", cv_autoremove),
         --  ("check     ", cv_check),
         --  ("clean     ", cv_clean),
         --  ("config    ", cv_config),
         --  ("create    ", cv_create),
         --  ("delete    ", cv_delete),
         --  ("fetch     ", cv_fetch),
         --  ("help      ", cv_help),
         --  ("info      ", cv_info),
         --  ("install   ", cv_install),
         --  ("lock      ", cv_lock),
         --  ("query     ", cv_query),
         --  ("register  ", cv_register),
         --  ("remove    ", cv_remove),
         --  ("repo      ", cv_repo),
         --  ("rquery    ", cv_rquery),
         --  ("search    ", cv_search),
         --  ("set       ", cv_set),
         --  ("shell     ", cv_shell),
         --  ("shlib     ", cv_shlib),
         --  ("ssh       ", cv_ssh),
         --  ("stats     ", cv_stats),
         --  ("unlock    ", cv_unlock),
         --  ("update    ", cv_update),
         --  ("upgrade   ", cv_upgrade),
         --  ("version   ", cv_version),
         --  ("which     ", cv_which)
        );

      bandolier : keyword_string := (others => ' ');
      Low       : Natural := all_keywords'First;
      High      : Natural := all_keywords'Last;
      Mid       : Natural;
   begin
      if component'Length > keyword_string'Length or else
        component'Length < 3
      then
         return cv_unset;
      end if;

      bandolier (1 .. component'Length) := component;

      loop
         Mid := (Low + High) / 2;
         if bandolier = all_keywords (Mid).keyword  then
            return all_keywords (Mid).keytype;
         elsif bandolier < all_keywords (Mid).keyword then
            exit when Low = Mid;
            High := Mid - 1;
         else
            exit when High = Mid;
            Low := Mid + 1;
         end if;
      end loop;
      return cv_unset;
   end get_command;



end Raven.Cmd.Line;
