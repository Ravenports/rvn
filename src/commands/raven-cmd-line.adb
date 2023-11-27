--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

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

      expanded_args : string_crate.Vector;
      last_cmd      : Clswitch := nothing_pending;

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
                  elsif datum = "-l" or else datum = "--list-files" then
                     data.cmd_info.list_files := True;
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

      --  TODO: when annotated implemented:
      --  check_annotate_stdin;

      --  TODO: when version re-implemented:
      --  check_version_stdin;

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
   procedure handle_trailing_pkgname (self : in out Cldata; datum : String; datumtxt : Text)
   is
      hyphen : constant Character := '-';
   begin
      if datum (datum'First) = hyphen then
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
      if self.command = cv_info then
         if not self.common_options.all_installed_pkgs then
            if IsBlank (self.cmd_info.path_archive_file) then
               self.common_options.all_installed_pkgs := True;
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
                              expanded_args.Append (SUS (word));
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
