--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Command_Line;
with Ada.Characters.Latin_1;
with Raven.Event;
with Raven.Cmd.Unset;
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

      expanded_args : string_crate.Vector;
      result        : Cldata;
      last_cmd      : Clbase_switch := nothing_pending;

      ------------------------
      --  translate_switch  --
      ------------------------
      procedure translate_switch (position : string_crate.Cursor)
      is
         datumtxt : Text renames string_crate.Element (position);
         datum : constant String := USS (datumtxt);
      begin
         if result.parse_error or else
           not IsBlank (result.next_argument)
         then
            return;
         end if;

         --  result.command is initialized as cv_unset
         if last_cmd = nothing_pending then
            if datum = "-d" or else datum = "--debug"
            then
               case result.pre_command.debug_setting is
                  when silent     => result.pre_command.debug_setting := high_level;
                  when high_level => result.pre_command.debug_setting := moderate;
                  when moderate   => result.pre_command.debug_setting := low_level;
                  when low_level  => null;
               end case;
            elsif datum = "-v" or else datum = "--version"
            then
               case result.pre_command.version_setting is
                  when not_shown  =>
                     result.pre_command.version_setting := just_version;
                  when just_version  =>
                     result.pre_command.version_setting := dump_configuration;
                  when dump_configuration =>
                     null;
               end case;
            elsif datum = "-l" or else datum = "--list"
            then
               result.pre_command.list_commands := True;
            elsif datum = "--status-check"
            then
               result.pre_command.status_check := True;
            elsif datum = "-4"
            then
               result.pre_command.internet_protocol := IPv4_only;
            elsif datum = "-6"
            then
               result.pre_command.internet_protocol := IPv6_only;
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
               --  This far means either we hit a secondary command (or alias) or
               --  we've got an unrecognized switch or command
               if datum (datum'First) = '-' then
                  set_error (result, "Unrecognized switch:" & datum);
               else
                  result.next_argument := SUS (datum);
               end if;
            end if;
         else
            --  insert second part of last seen command
            case last_cmd is
               when nothing_pending    => null;   --  impossible
               when global_chroot      => result.pre_command.chroot_first      := datumtxt;
               when global_config      => result.pre_command.custom_configfile := datumtxt;
               when global_repoconfdir => result.pre_command.custom_repos_dir  := datumtxt;
               when global_rootdir     => result.pre_command.install_rootdir   := datumtxt;
               when global_option =>
                  if IsBlank (result.pre_command.option_nvpairs) then
                     result.pre_command.option_nvpairs := datumtxt;
                  else
                     SU.Append (result.pre_command.option_nvpairs, LAT.Vertical_Line & datum);
                  end if;
            end case;
            last_cmd := nothing_pending;
         end if;

      end translate_switch;

   begin
      expand_command_line (expanded_args, 1);
      expanded_args.Iterate (translate_switch'Access);
      case last_cmd is
         when nothing_pending => null;
         when others => result.pending_argument := True;
      end case;
      return result;
   end parse_command_line;


   -------------------------------
   --  parse_secondary_command  --
   -------------------------------
   procedure parse_secondary_command (data : in out Cldata)
   is
      procedure translate_switch (position : string_crate.Cursor);
      procedure check_version_stdin;

      expanded_args : string_crate.Vector;
      last_cmd      : Clswitch := nothing_pending;

      ---------------------------
      --  check_version_stdin  --
      ---------------------------
      procedure check_version_stdin
      is
         --  stdin can be used for either the pattern or the package name, but not both
         --  multiple inputs supported - they are delimited with character of value 0
         procedure push (mychar : Character) is
         begin
            if data.cmd_version.hyphen1 then
               SU.Append (data.cmd_version.test1, mychar);
            else
               SU.Append (data.cmd_version.test2, mychar);
            end if;
         end push;
      begin
         if data.cmd_version.behavior = compare_against_pattern then
            data.cmd_version.hyphen1 := equivalent (data.cmd_version.test1, "-");
            data.cmd_version.hyphen2 := equivalent (data.cmd_version.test2, "-");
            if data.cmd_version.hyphen1 and then data.cmd_version.hyphen2 then
               set_error (data, "Only one input can be set through standard-in stream");
               return;
            end if;
            if data.cmd_version.hyphen1 or else data.cmd_version.hyphen2 then
               declare
                  c : Character;
               begin
                  if data.cmd_version.hyphen1 then
                     data.cmd_version.test1 := blank;
                  else
                     data.cmd_version.test2 := blank;
                  end if;
                  while not TIO.End_Of_File loop
                     TIO.Get (c);
                     if c = ' ' then
                        c := Character'Val (0);
                     end if;
                     push (c);
                     if TIO.End_Of_Line then
                        push (Character'Val (0));
                     end if;
                  end loop;
               end;
            end if;
         end if;
      end check_version_stdin;

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
         error_ill  : constant String := "Illegal option -- ";
         error_rec  : constant String := "Unrecognized option: ";
         error_exp  : constant String := "Unexpected argument: ";
         error_chk  : constant String := "Attempt to redefine check action: ";
         error_ann  : constant String := "Attempt to redefine annotation action: ";
         error_PR   : constant String := "The --provides and --requires" & AME;
         error_like : constant String := "The --like and --not-like" & AME;
         hyphen     : constant Character := '-';

      begin
         if data.parse_error then
            return;
         end if;

         if last_cmd = nothing_pending then
            case data.command is
               when cv_unset =>
                  --  We've already processed the pre-command switches.
                  --  We only need to determine what command this was.
                  data.command := get_command (datum);
                  if data.command = cv_unset then
                     set_error (data, error_rec & datum);
                  elsif data.command = cv_help then
                     last_cmd := help;
                  end if;

               when cv_alias =>
                  if datum = sws_quiet or else datum = swl_quiet then
                     data.common_options.quiet := True;
                  elsif datum = "-l" or else datum = "--list" then
                     data.cmd_alias.without_args := True;
                  elsif datum (datum'First) = '-' then
                     --  -x (illegal) --xx (unrecognized)  - (unrecognized) -- (unrecognized)
                     if datum'Length = 1 or else
                       datum (datum'First .. datum'First + 1) = "--"
                     then
                        set_error (data, error_rec & datum);
                     else
                        set_error (data, error_ill & datum (datum'First + 1 .. datum'Last));
                     end if;
                  else
                     if IsBlank (data.cmd_alias.alias) then
                        data.cmd_alias.alias := datumtxt;
                     else
                        SU.Append (data.cmd_alias.alias, Character'Val (0) & datum);
                     end if;
                  end if;

               when cv_config =>
                  if IsBlank (data.cmd_config.key) then
                     data.cmd_config.key := datumtxt;
                  else
                     set_error (data, "Only one config key is permitted.");
                  end if;

               when cv_create =>
                  if datum = sws_quiet or else datum = swl_quiet then
                     data.common_options.quiet := True;
                  elsif datum = sws_verb or else datum = swl_verb then
                     data.common_options.verbose := True;
                  elsif datum = "-p" or else datum = "--prefix" then
                     last_cmd := create_prefix;
                  elsif datum = "-r" or else datum = "--root-dir" then
                     last_cmd := create_rootdir;
                  elsif datum = "-m" or else datum = "--metadata" then
                     last_cmd := create_metadata;
                  elsif datum = "-w" or else datum = "--whitelist" then
                     last_cmd := create_whitelist;
                  elsif datum = "-o" or else datum = "--out-dir" then
                     last_cmd := create_outdir;
                  elsif datum = "-t" or else datum = "--timestamp" then
                     last_cmd := create_timestamp;
                  else
                     handle_trailing_pkgname (data, datum, datumtxt);
                  end if;

               when cv_help =>
                  if data.help_command = cv_unset and then
                    data.help_command2 = cv2_unset
                  then
                     last_cmd := help;
                  else
                     set_error (data, "The help command only takes one argument");
                  end if;

               when cv_info =>
                  if datum = sws_quiet or else datum = swl_quiet then
                     data.common_options.quiet := True;
                  elsif aCgix (data, datum) then
                     null;
                  elsif datum = "-A" or else datum = "--annotations" then
                     data.cmd_info.annotations := True;
                  elsif datum = "-B" or else datum = "--required-shlibs" then
                     data.cmd_info.shlibs_used := True;
                  elsif datum = "-b" or else datum = "--provided-shlibs" then
                     data.cmd_info.shlibs_provided := True;
                  elsif datum = "-j" or else datum = "--adjacent-shlibs" then
                     data.cmd_info.shlibs_adjacent := True;
                  elsif datum = "-M" or else datum = "--pkg-message" then
                     data.cmd_info.install_message := True;
                  elsif datum = "-D" or else datum = "--description" then
                     data.cmd_info.description := True;
                  elsif datum = "-d" or else datum = "--dependencies" then
                     data.cmd_info.dependencies := True;
                  elsif datum = "-e" or else datum = "--exists" then
                     data.cmd_info.installed := True;
                  elsif datum = "-f" or else datum = "--full" then
                     data.cmd_info.full_information := True;
                  elsif datum = "-I" or else datum = "--comment" then
                     data.cmd_info.comment := True;
                  elsif datum = "-L" or else datum = "--list-digests" then
                     data.cmd_info.list_digests := True;
                  elsif datum = "-l" or else datum = "--list-files" then
                     data.cmd_info.list_files := True;
                  elsif datum = "-X" or else datum = "--list-extended" then
                     data.cmd_info.list_attributes := True;
                  elsif datum = "-p" or else datum = "--prefix" then
                     data.cmd_info.install_prefix := True;
                  elsif datum = "-R" or else datum = "--raw" then
                     data.cmd_info.raw_manifest := True;
                  elsif datum = "-r" or else datum = "--required-by" then
                     data.cmd_info.rev_deps := True;
                  elsif datum = "-s" or else datum = "--size" then
                     data.cmd_info.total_size := True;
                  elsif datum = "-N" or else datum = "--namebase" then
                     data.cmd_info.namebase := True;
                  elsif datum = "-S" or else datum = "--subpackage" then
                     data.cmd_info.subpackage := True;
                  elsif datum = "-V" or else datum = "--variant" then
                     data.cmd_info.variant := True;
                  elsif datum = "-F" or else datum = "--file" then
                     last_cmd := info_archive_file;
                  else
                     handle_trailing_pkgname (data, datum, datumtxt);
                     if not IsBlank (data.cmd_info.path_archive_file) then
                        set_error (data, "Use of -F switch with pkg-name is not permitted.");
                     end if;
                  end if;

               when cv_install =>
                  if datum = sws_quiet or else datum = swl_quiet then
                     data.common_options.quiet := True;
                  elsif datum = sws_yes or else datum = swl_yes then
                     data.common_options.assume_yes := True;
                  elsif datum = sws_dryrun or else datum = swl_dryrun then
                     data.common_options.dry_run := True;
                  elsif datum = sws_nocat or else datum = swl_nocat then
                     data.common_options.no_repo_update := True;
                  elsif aCgix (data, datum, False) then
                     null;
                  elsif datum = "-A" or else datum = "--automatic" then
                     data.cmd_install.automatic := True;
                  elsif datum = "-F" or else datum = "--fetch-only" then
                     data.cmd_install.fetch_only := True;
                  elsif datum = "-f" or else datum = "--force" then
                     data.cmd_install.force_install := True;
                  elsif datum = "-I" or else datum = "--no-scripts" then
                     data.cmd_install.inhibit_scripts := True;
                  elsif datum = "-M" or else datum = "--ignore-missing" then
                     data.cmd_install.ignore_missing := True;
                  elsif datum = "-R" or else datum = "--recursive" then
                     data.cmd_install.recursive := True;
                  elsif datum = "--file" then
                     data.cmd_install.local_file := True;
                  elsif datum = "--no-registration" then
                     data.cmd_install.no_register := True;
                  elsif datum = "--only-registration" then
                     data.cmd_install.only_register := True;
                  elsif datum = sws_repo or else datum = swl_repo then
                     last_cmd := generic_repo_name;
                  else
                     handle_pkg_patterns (data, datum, datumtxt);
                  end if;

               when cv_shell =>
                  data.cmd_shell.pass_arguments.Append (datumtxt);

               when cv_shlib =>
                  if datum = sws_quiet or else datum = swl_quiet then
                     data.common_options.quiet := True;
                  elsif datum = "-P" or else datum = "--provides" then
                     data.cmd_shlib.provides := True;
                  elsif datum = "-R" or else datum = "--requires" then
                     data.cmd_shlib.requires := True;
                  else
                     handle_trailing_pkgname (data, datum, datumtxt);
                  end if;

               when cv_which =>
                  if datum = sws_quiet or else datum = swl_quiet then
                     data.common_options.quiet := True;
                  elsif datum = "-g" or else datum = "--glob" then
                     data.common_options.shell_glob := True;
                  elsif datum = "-m" or else datum = "--show-match" then
                     data.cmd_which.show_match := True;
                  elsif datum = "-p" or else datum = "--path-search" then
                     data.cmd_which.path_search := True;
                  else
                     handle_trailing_pkgname (data, datum, datumtxt);
                  end if;

               when cv_version =>
                  if datum = sws_quiet or else datum = swl_quiet then
                     data.common_options.quiet := True;
                  elsif datum = sws_verb or else datum = swl_verb then
                     data.common_options.verbose := True;
                  elsif datum = sws_repo or else datum = swl_repo then
                     last_cmd := generic_repo_name;
                     if data.cmd_version.behavior = no_defined_behavior or else
                       data.cmd_version.behavior = use_remote_catalog_state
                     then
                        data.cmd_version.behavior := use_remote_catalog_state;
                     else
                        set_error (data, "The -r switch is not compatible with " &
                                     "-S, -I, -t, or -T switches.");
                     end if;
                  elsif aCgix (data, datum) then
                     null;
                  elsif datum = "-e" or else datum = "--exact" then
                     data.cmd_version.exact_match := True;
                  elsif datum = "-l" or else datum = "--like" then
                     if data.cmd_version.not_char /= Character'First then
                        set_error (data, error_like);
                     end if;
                     last_cmd := version_match_char;
                  elsif datum = "-L" or else datum = "--not-like" then
                     if data.cmd_version.match_char /= Character'First then
                        set_error (data, error_like);
                     end if;
                     last_cmd := version_not_char;
                  elsif datum = "-n" or else datum = "--match-name" then
                     last_cmd := version_pkgname;
                  elsif datum = "-t" or else datum = "--test-version" then
                     if data.cmd_version.behavior = no_defined_behavior then
                        data.cmd_version.behavior := test_versions;
                     else
                        set_error (data, "The --test-version switch is not compatible with " &
                                   "-S, -I, -R, -r, or -T switches.");
                     end if;
                  elsif datum = "-T" or else datum = "--test-pattern" then
                     if data.cmd_version.behavior = no_defined_behavior then
                        data.cmd_version.behavior := compare_against_pattern;
                     else
                        set_error (data, "The --test-pattern switch is not compatible with " &
                                   "-S, -I, -R, -r, or -t switches.");
                     end if;
                  elsif datum = "-S" or else datum = "--snapshot" then
                     if data.cmd_version.behavior = no_defined_behavior then
                        data.cmd_version.behavior := use_rvnindex_snapshot;
                     else
                        set_error (data, "The -I switch is not compatible with " &
                                     "-I, -R, -r, -T, or -t switches.");
                     end if;
                  elsif datum = "-I" or else datum = "--index" then
                     if data.cmd_version.behavior = no_defined_behavior then
                        data.cmd_version.behavior := use_rvnindex_release;
                     else
                        set_error (data, "The -I switch is not compatible with " &
                                     "-S, -R, -r, -T, or -t switches.");
                     end if;
                  elsif datum = "-R" or else datum = "--remote" then
                     if data.cmd_version.behavior = no_defined_behavior or else
                       data.cmd_version.behavior = use_remote_catalog_state
                     then
                        data.cmd_version.behavior := use_remote_catalog_state;
                     else
                        set_error (data, "The -R switch is not compatible with " &
                                     "-S, -I, -T, or -t switches.");
                     end if;
                  else
                     if data.cmd_version.behavior = compare_against_pattern or else
                       data.cmd_version.behavior = test_versions
                     then
                        if IsBlank (data.cmd_version.test1) then
                           data.cmd_version.test1 := datumtxt;
                        elsif IsBlank (data.cmd_version.test2) then
                           data.cmd_version.test2 := datumtxt;
                        else
                           set_error (data, "Too many arguments.");
                        end if;
                     else
                        handle_trailing_pkgname (data, datum, datumtxt);
                     end if;
                  end if;
               when cv_genrepo =>
                  if datum = sws_quiet or else datum = swl_quiet then
                     data.common_options.quiet := True;
                  elsif datum = "-k" or else datum = "--key" then
                     last_cmd := genrepo_key;
                  elsif datum = "-p" or else datum = "--pubkey" then
                     last_cmd := genrepo_pubkey;
                  elsif datum = "-x" or else datum = "--external" then
                     last_cmd := genrepo_sign_cmd;
                  elsif datum = "-f" or else datum = "--fingerprint" then
                     last_cmd := genrepo_finger;
                  else
                     handle_trailing_pkgname (data, datum, datumtxt);
                  end if;
               when cv_catalog =>
                  if datum = sws_quiet or else datum = swl_quiet then
                     data.common_options.quiet := True;
                  elsif datum = "-f" or else datum = "--force" then
                     data.cmd_catalog.force_update := True;
                  end if;
               when cv_clean =>
                  if datum = sws_quiet or else datum = swl_quiet then
                     data.common_options.quiet := True;
                  elsif datum = sws_yes or else datum = swl_yes then
                     data.common_options.assume_yes := True;
                  elsif datum = sws_dryrun or else datum = swl_dryrun then
                     data.common_options.dry_run := True;
                  elsif datum = "-a" or else datum = "--all" then
                     data.cmd_clean.delete_all := True;
                  end if;
            end case;
         else
            --  insert second part of last seen command
            case last_cmd is
               when nothing_pending    => null;   --  impossible
               when create_metadata    => data.cmd_create.metadata_file   := datumtxt;
               when create_whitelist   => data.cmd_create.whitelist_file  := datumtxt;
               when create_rootdir     => data.cmd_create.rootdir_dir     := datumtxt;
               when create_outdir      => data.cmd_create.output_dir      := datumtxt;
               when create_timestamp   => data.cmd_create.timestamp       := datumtxt;
               when create_prefix      => data.cmd_create.prefix          := datumtxt;
               when info_archive_file  => data.cmd_info.path_archive_file := datumtxt;
               when generic_repo_name  => data.common_options.repo_name   := datumtxt;
               when version_match_char => data.cmd_version.match_char     := datum (datum'First);
               when version_not_char   => data.cmd_version.not_char       := datum (datum'First);
               when version_pkgname    => data.cmd_version.pkg_name       := datumtxt;
               when genrepo_key        => data.cmd_genrepo.key_private    := datumtxt;
               when genrepo_pubkey     => data.cmd_genrepo.key_public     := datumtxt;
               when genrepo_sign_cmd   => data.cmd_genrepo.sign_command   := datumtxt;
               when genrepo_finger     => data.cmd_genrepo.fprint_file    := datumtxt;
               when help =>
                  data.help_command := get_command (datum);
                  if data.help_command = cv_unset then
                     if datum = progname then
                        data.help_command2 := cv2_main;
                     elsif datum = progname & ".conf" then
                        data.help_command2 := cv2_main_conf;
                     elsif datum = "repository" then
                        data.help_command2 := cv2_repository;
                     else
                        set_error (data, SQ (datum) & " is not a recognized command");
                     end if;
                  end if;
            end case;
            last_cmd := nothing_pending;
         end if;
      end translate_switch;
   begin
      second_expansion (expanded_args, USS (data.next_argument));
      expanded_args.Iterate (translate_switch'Access);

      case last_cmd is
         when nothing_pending => null;
         when others =>
            set_error (data, "The last switch requires an argument");
      end case;

      --  TODO: when annotated implemented:
      --  check_annotate_stdin;

      check_version_stdin;

      --  TODO: when rquery implemented:
      --  check_implied_rquery_all;

      --  TODO: when query implemented:
      --  check_implied_query_all;

      --  TODO:  check_stats_default ??

      check_create_incompatibilities (data);
      check_implied_info_all (data);

   end parse_secondary_command;


   ---------------------------
   --  expand_command_line  --
   ---------------------------
   procedure expand_command_line
     (expanded_args : in out string_crate.Vector;
      start_argnum : Positive) is
   begin
      for Arg in start_argnum .. CLI.Argument_Count loop
         declare
            datum      : constant String := CLI.Argument (Arg);
            save_as_is : Boolean := False;
         begin
            if datum'Length = 1 then
               --  Too small to be any kind of switch, just save it
               --  It also could be a hyphen to indicate stdin output
               save_as_is := True;
            elsif datum'Length > 0 and then
              datum (datum'First) = '-'
            then
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
         ("alias     ", cv_alias),
         ("catalog   ", cv_catalog),
         ("clean     ", cv_clean),
         ("config    ", cv_config),
         ("create    ", cv_create),
         ("genrepo   ", cv_genrepo),
         ("help      ", cv_help),
         ("info      ", cv_info),
         ("install   ", cv_install),
         ("shell     ", cv_shell),
         ("shlib     ", cv_shlib),
         ("version   ", cv_version),
         ("which     ", cv_which)

         --  ("add       ", cv_add),
         --  ("annotate  ", cv_annotate),
         --  ("autoremove", cv_autoremove),
         --  ("check     ", cv_check),
         --  ("fetch     ", cv_fetch),
         --  ("lock      ", cv_lock),
         --  ("query     ", cv_query),
         --  ("register  ", cv_register),
         --  ("remove    ", cv_remove),
         --  ("rquery    ", cv_rquery),
         --  ("search    ", cv_search),
         --  ("set       ", cv_set),
         --  ("ssh       ", cv_ssh),
         --  ("stats     ", cv_stats),
         --  ("unlock    ", cv_unlock),
         --  ("upgrade   ", cv_upgrade),
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


   -----------------
   --  set_error  --
   -----------------
   procedure set_error (self : in out Cldata; error_msg : String)
   is
   begin
      --  Don't overwrite previous errors
      if not self.parse_error then
         self.parse_error := True;
         self.error_message := SUS (error_msg);
      end if;
   end set_error;


   -------------------------------
   --  handle_trailing_pkgname  --
   -------------------------------
   procedure handle_trailing_pkgname (self : in out Cldata; datum : String; datumtxt : Text) is
   begin
      if datum (datum'First) = LAT.Hyphen then
         set_error (self, "Unexpected switch: " & datum);
      else
         if self.common_options.all_installed_pkgs then
            set_error (self, "Unexpected file pattern with --all option: " & datum);
         elsif not IsBlank (self.common_options.name_pattern) then
            set_error (self, "Attempt to redefine file name pattern from "
                       & SQ (USS (self.common_options.name_pattern)) & " to " & DQ (datum));
         else
            self.common_options.name_pattern := datumtxt;
         end if;
      end if;
   end handle_trailing_pkgname;


   ---------------------------
   --  handle_pkg_patterns  --
   ---------------------------
   procedure handle_pkg_patterns (self : in out Cldata; datum : String; datumtxt : Text) is
   begin
      if datum (datum'First) = LAT.Hyphen then
         set_error (self, "Unexpected switch: " & datum);
         return;
      end if;
      self.common_options.multiple_patterns.Append (datumtxt);
   end handle_pkg_patterns;


   -------------
   --  aCgix  --
   -------------
   function aCgix (self : in out Cldata; datum : String; use_all : Boolean := True) return Boolean
   is
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
   begin
      --  -C/-i are mutually exclusive (both affect same setting)
      --  Actually all switches are mutually exclusive to the others
      --  -C > -i > -g > -r

      if use_all and then (datum = sws_all or else datum = swl_all) then
         self.common_options.all_installed_pkgs := True;
      elsif datum = sws_case or else datum = swl_case then
         self.common_options.case_sensitive := True;
      elsif datum = sws_icase or else datum = swl_icase then
         self.common_options.case_insensitive := True;
      elsif datum = sws_glob or else datum = swl_glob then
         self.common_options.shell_glob := True;
      elsif datum = sws_regex or else datum = swl_regex then
         self.common_options.regex := True;
      else
         return False;
      end if;
      return True;
   end aCgix;


   --------------------------------------
   --  check_create_incompatibilities  --
   --------------------------------------
   procedure check_create_incompatibilities (self : in out Cldata)
   is
      --  This is kind of a legacy check for pkg users.  Probably not really necessary.
   begin
      if self.command = cv_create then
         if self.common_options.all_installed_pkgs or else
           self.common_options.shell_glob or else
           self.common_options.regex
         then
            set_error (self, "Switches -a, -g, -x invalid - create requires descrete filename");
         end if;
      end if;
   end check_create_incompatibilities;


   ------------------------------
   --  check_implied_info_all  --
   ------------------------------
   procedure check_implied_info_all (self : in out Cldata) is
   begin
      --  These command imply -a
      --  rvn info
      --  any rvn info missing the pkg-name / pattern argument
      --  -a with pkg-name is an error
      if self.command = cv_info then
         if self.common_options.all_installed_pkgs then
            if not IsBlank (self.cmd_info.path_archive_file) then
               set_error (self, "--all is mutually exclusive with pkg-name");
            end if;
         else
            if IsBlank (self.cmd_info.path_archive_file) then
               if isBlank (self.common_options.name_pattern) then
                  self.common_options.all_installed_pkgs := True;
               end if;
               if self.cmd_info.list_attributes then
                  set_error (self, "--list-extended is not available for installed packages");
               end if;
            else
               if self.common_options.shell_glob or else
                 self.common_options.regex or else
                 self.common_options.case_insensitive or else
                 self.common_options.case_insensitive
               then
                  set_error (self, "-Cgix switch settings are invalid with pkg-file");
               end if;
            end if;
         end if;
      end if;
   end check_implied_info_all;


   ----------------------------------
   --  pending_command_recognized  --
   ----------------------------------
   function pending_command_recognized (data : Cldata) return Boolean
   is
      --  This command must be run after program initialization so that
      --  the aliases from the configuration files are loaded.

      verb : Command_verb;
      pending_command : constant String := USS (data.next_argument);
   begin
      verb := get_command (pending_command);
      case verb is
         when cv_unset =>
            if IsBlank (Raven.Cmd.Unset.alias_definition (pending_command)) then
               return False;
            end if;
         when others =>
            null;
      end case;
      return True;
   end pending_command_recognized;


   ------------------------
   --  second_expansion  --
   ------------------------
   procedure second_expansion
     (expanded_args : in out string_crate.Vector;
      next_command  : String)
   is
      function concatenate (full : String; first_chunk, last_chunk : Positive) return text;
      procedure push (S : String);

      verb : constant Command_verb := get_command (next_command);
      arg_index : Positive := 1;
      found : Boolean := False;

      function concatenate (full : String; first_chunk, last_chunk : Positive) return text
      is
         result : text := SUS (specific_field (full, first_chunk));
         ndex : Positive := first_chunk;
      begin
         loop
            ndex := ndex + 1;
            exit when ndex > last_chunk;
            SU.Append (result, LAT.Space & specific_field (full, ndex));
         end loop;
         declare
            resultstr : constant String := USS (result);
         begin
            return (SUS (resultstr (resultstr'First + 1 .. resultstr'Last - 1)));
         end;
      end concatenate;

      procedure push (S : String)
      is
         --  not intended for concatenated chunks
         --  It is to handle cases like "-sq" which needs two pushes, e.g. "-s" and "-q"
         --  It only applies with a single leading hyphen.
         len : Natural := S'Length;
      begin
         if len > 2 then
            if S (S'First) = LAT.Hyphen and then
              S (S'First + 1) /= LAT.Hyphen
            then
               for char in S'First + 1 .. S'Last loop
                  expanded_args.Append (SUS ('-' & S (char)));
               end loop;
               return;
            end if;
         end if;
         expanded_args.Append (SUS (s));
      end push;
   begin
      --  The next_command must match one of the arguments (fatal error if it doesn't)
      loop
         if CLI.Argument (arg_index) = next_command then
            --  next_command is known command, not an alias.
            --  Use existing expansion procedure and return.
            case verb is
               when cv_unset =>
                  --  must be an alias
                  found := True;
                  exit;
               when others =>
                  expand_command_line (expanded_args, arg_index);
                  return;
            end case;
         end if;
         arg_index := arg_index + 1;
         exit when arg_index > CLI.Argument_Count;
      end loop;

      if not found then
         Raven.Event.emit_debug
              (high_level, "second_expansion failed. " & next_command & " not found in arguments"
               & " (should have been impossible)");
         return;
      end if;

      declare
         definition : constant String := Trim (Raven.Cmd.Unset.alias_definition (next_command));
         open_quote : Natural := 0;
         num_chunks : Natural;
         mark_chunk : Natural := 0;
      begin
         if IsBlank (definition) then
            Raven.Event.emit_debug
              (high_level, "second_expansion failed. " & next_command
               & " alias definition is blank");
            return;
         end if;

         --  Process in chunks.
         --  If there is no open quote and the chunk doesn't start with a quote, push it.
         --  If there is an an open quote and the chunk doesn't end in the same quote, continue.
         --  If there is an oan open quote and the check ends in the same quote, push the
         --     concatenated chunks as one.
         --  if there is no open quoate and the chunk starts with a quote, continue
         --  If the loop ends and a quote is open, push what we have.

         num_chunks := count_char (definition, LAT.Space) + 1;
         for chunk in 1 .. num_chunks loop
            declare
               word : constant String := specific_field (definition, chunk);
               char : Character;
            begin
               --  word length could be zero if we have adjacent space characters
               if word'Length > 0 then
                  case open_quote is
                     when 0 =>
                        char := word (word'First);
                        case char is
                           when LAT.Apostrophe =>
                              open_quote := 1;
                              mark_chunk := chunk;
                           when LAT.Quotation =>
                              open_quote := 2;
                              mark_chunk := chunk;
                           when others =>
                              push (word);
                        end case;
                     when 1 =>
                        char := word (word'Last);
                        case char is
                           when LAT.Apostrophe =>
                              open_quote := 0;
                              expanded_args.append (concatenate (definition, mark_chunk, chunk));
                           when others =>
                              null;
                        end case;
                     when 2 =>
                        char := word (word'Last);
                        case char is
                           when LAT.Quotation =>
                              open_quote := 0;
                              expanded_args.append (concatenate (definition, mark_chunk, chunk));
                           when others =>
                              null;
                        end case;
                     when others =>
                        null;  --  not possible
                  end case;
               end if;
            end;
         end loop;
         case open_quote is
            when 1 | 2 =>
               --  User forgot the close the brackets, just do it now
               expanded_args.append (concatenate (definition, mark_chunk, num_chunks));
            when others =>
               null;
         end case;
      end;

      --  Get any arguments after the alias definition
      if arg_index + 1 <= CLI.Argument_Count then
         expand_command_line (expanded_args, arg_index + 1);
      end if;

   end second_expansion;

end Raven.Cmd.Line;
