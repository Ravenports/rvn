--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

package Raven.Cmd.Line is

   --  Parse the first part of the command line.
   function parse_command_line return Cldata;

   --  Returns true if the pending command is a recognized rvn command or if
   --  matches a known alias
   function pending_command_recognized (data : Cldata) return Boolean;

   --  The next_command has been confirmed to be either a known command or an alias
   --  Parse the second part of the command line
   procedure parse_secondary_command (data : in out Cldata);

private

   function get_command (component : String) return Command_verb;
   procedure set_error (self : in out Cldata; error_msg : String);
   procedure handle_trailing_pkgname (self : in out Cldata; datum : String; datumtxt : Text);
   function aCgix (self : in out Cldata; datum : String; use_all : Boolean := True) return Boolean;
   procedure check_create_incompatibilities (self : in out Cldata);
   procedure check_implied_info_all (self : in out Cldata);

   procedure expand_command_line
     (expanded_args : in out string_crate.Vector;
      start_argnum : Positive);

   procedure second_expansion
     (expanded_args : in out string_crate.Vector;
      next_command  : String);

   type Clbase_switch is
     (nothing_pending,
      global_chroot,
      global_config,
      global_repoconfdir,
      global_rootdir,
      global_option);

   type Clswitch is
        (nothing_pending,
         --  generic_repo_name,
         --  generic_raw_format,
         --  annotate_tag,
         create_metadata,
         create_whitelist,
         create_outdir,
         create_rootdir,
         create_timestamp,
         create_prefix,
         --  fetch_destdir,
         help,
         info_archive_file --,
         --  query_condition,
         --  query_filename,
         --  repo_meta_File,
         --  repo_outdir,
         --  repo_signing_cmd,
         --  rquery_eval_cond,
         --  search_modifier,
         --  search_field,
         --  search_label,
         --  set_automatic,
         --  set_vital,
         --  set_change_name,
         --  version_match_char,
         --  version_not_char,
         --  version_origin,
         --  version_pkgname,
         --  which_filename
        );

end Raven.Cmd.Line;
