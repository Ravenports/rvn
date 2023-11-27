--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

package Raven.Cmd.Line is

   function parse_command_line return Cldata;

private

   procedure expand_command_line (expanded_args : in out string_crate.Vector);
   function get_command (component : String) return Command_verb;

   type Clswitch is
        (nothing_pending,
         global_chroot,
         global_config,
         global_repoconfdir,
         global_rootdir,
         global_option,
         --  generic_repo_name,
         --  generic_raw_format,
         --  annotate_tag,
         create_metadata,
         create_whitelist,
         create_outdir,
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
