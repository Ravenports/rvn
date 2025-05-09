--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Text_IO;
with Ada.Containers.Vectors;
with Raven.Pkgtypes;

package Raven.Cmd is

   package TIO renames Ada.Text_IO;
   package CON renames Ada.Containers;

   type Cldata is tagged private;

private

   package string_crate is new CON.Vectors
     (Element_Type => Text,
      Index_Type   => Natural,
      "="          => SU."=");

   --  Keep this in alphabetical order after cv_unset
   type Command_verb is
     (cv_unset,
      cv_audit,
      cv_alias,
      cv_annotate,
      cv_autoremove,
      cv_catalog,
      cv_check,
      cv_clean,
      cv_config,
      cv_create,
      cv_fetch,
      cv_genrepo,
      cv_help,
      cv_info,
      cv_install,
      cv_query,
      cv_remove,
      cv_rquery,
      cv_search,
      cv_shell,
      cv_shlib,
      cv_stats,
      cv_upgrade,
      cv_version,
      cv_which
     );

   type Command2_verb is
     (cv2_unset,
      cv2_main,
      cv2_repository,
      cv2_keywords,
      cv2_scripts,
      cv2_luascripts,
      cv2_query_format,
      cv2_ravensign,
      cv2_main_conf
     );

   type Versioning_Behavior is
     (no_defined_behavior,
      use_remote_catalog_state,
      use_rvnindex_snapshot,
      use_rvnindex_release,
      test_versions,
      compare_against_pattern
     );

   type version_depth is (not_shown, just_version, dump_configuration);
   type search_type is (search_unset, namebase, triplet, comment, description);
   subtype ST_Version is Natural range 0 .. 2;

   type switches_common is
      record
         verbose            : Boolean := False;
         quiet              : Boolean := False;  --  -q, --quiet
         case_sensitive     : Boolean := False;  --  -c, --case-sensitive
         exact_match        : Boolean := False;  --  -e, --exact
         dry_run            : Boolean := False;  --  -n, --dry-run
         assume_yes         : Boolean := False;  --  -y, --yes
         all_installed_pkgs : Boolean := False;  --  -a, --all
         no_repo_update     : Boolean := False;  --  -U, --no-repo-update
         name_pattern       : Text;
         repo_name          : Text;
      end record;

   type switches_alias_cmd is
      record
         without_args : Boolean := False;
         alias        : text;
      end record;

   type switches_config_cmd is
      record
         key : Text;
      end record;

   type switches_create_cmd is
      record
         output_dir      : Text;
         rootdir_dir     : Text;
         metadata_file   : Text;
         whitelist_file  : Text;
         timestamp       : Text;
         prefix          : Text;
      end record;

   type switches_shlib_cmd is
      record
         provides : Boolean := False;
         requires : Boolean := False;
      end record;

   type switches_stats_cmd is
      record
         catalog_only : Boolean := False;
         local_only   : Boolean := False;
         cache_only   : Boolean := False;
      end record;

   type switches_which_cmd is
      record
         show_match  : Boolean := False;
         path_search : Boolean := False;
         glob_input  : Boolean := False;
      end record;

   type switches_info_cmd is
      record
         annotations        : Boolean := False;  --  -A, --annotations
         shlibs_provided    : Boolean := False;  --  -B, --required-shlibs
         shlibs_used        : Boolean := False;  --  -b, --provided-shlibs
         shlibs_adjacent    : Boolean := False;  --  -j, --adjacent-shlibs
         install_message    : Boolean := False;  --  -M, --install-message
         remove_message     : Boolean := False;  --  -Z, --remove-message
         description        : Boolean := False;  --  -D, --description
         dependencies       : Boolean := False;  --  -d, --dependencies (package depends on them)
         installed          : Boolean := False;  --  -e, --exists
         full_information   : Boolean := False;  --  -f, --full
         comment            : Boolean := False;  --  -I, --comment
         list_digests       : Boolean := False;  --  -L, --list-digests
         list_files         : Boolean := False;  --  -l, --list-files
         list_attributes    : Boolean := False;  --  -X, --list-extended
         options            : Boolean := False;  --  -o, --options
         install_prefix     : Boolean := False;  --  -p, --prefix
         raw_manifest       : Boolean := False;  --  -R, --raw
         rev_deps           : Boolean := False;  --  -r, --required-by (list pkgs that require it)
         total_size         : Boolean := False;  --  -s, --size
         abi                : Boolean := False;  --  -w, --abi
         namebase           : Boolean := False;  --  -N, --namebase
         subpackage         : Boolean := False;  --  -S, --subpackage
         variant            : Boolean := False;  --  -V, --variant
         version            : Boolean := False;  --  -v, --version
         catalog            : Boolean := False;  --  -K, --catalog
         path_archive_file  : Text;
         glob_input         : Boolean := False;  --  -g, --glob
      end record;

   type switches_install_cmd is
      record
         automatic          : Boolean := False;  --  -A, --automatic
         manual             : Boolean := False;  --  -M, --manual
         fetch_only         : Boolean := False;  --  -F, --fetch-only
         force_install      : Boolean := False;  --  -f, --force
         inhibit_scripts    : Boolean := False;  --  -I, --no-scripts
         drop_depends       : Boolean := False;  --  -d, --drop_depends
         local_file         : Boolean := False;  --      --file
         no_register        : Boolean := False;          --no-register
         only_register      : Boolean := False;          --only-register
         name_patterns      : Pkgtypes.Text_List.Vector;
      end record;

   type switches_upgrade_cmd is
      record
         fetch_only         : Boolean := False;  --  -F, --fetch-only
         force_install      : Boolean := False;  --  -f, --force
         inhibit_scripts    : Boolean := False;  --  -I, --no-scripts
         drop_depends       : Boolean := False;  --  -d, --drop_depends
         name_patterns      : Pkgtypes.Text_List.Vector;
      end record;

   type switches_shell_cmd is
      record
         pass_arguments     : string_crate.Vector;
      end record;

   type switches_version_cmd is
      record
         behavior     : Versioning_Behavior := no_defined_behavior;
         match_char   : Character := Character'First;
         not_char     : Character := Character'First;
         test1        : Text;
         test2        : Text;
         hyphen1      : Boolean := False;
         hyphen2      : Boolean := False;
      end record;

   type switches_genrepo_cmd is
      record
         key_private  : Text;
         key_public   : Text;
         sign_command : Text;
         fprint_file  : Text;
      end record;

   type switches_catalog_cmd is
      record
         force_update : Boolean := False;
      end record;

   type switches_clean_cmd is
      record
         delete_all   : Boolean := False;
      end record;

   type switches_query_cmd is
      record
         evaluate     : Text;
         query_format : Text;
      end record;

   type switches_rquery_cmd is
      record
         evaluate     : Text;
         query_format : Text;
      end record;

   type Modifier_Data is
     (annotations, abi, categories, comment, dependencies, description, full, licenses,
      maintainer, namebase, options, prefix, required_by, rvnsize, shlibs_prov, shlibs_req,
      size, subpackage, variant, pkgversion, www);

   type Modifier_Set is array (Modifier_Data'Range) of Boolean;

   type switches_search_cmd is
      record
         search        : search_type := search_unset;
         glob_input    : Boolean := False;
         num_modifiers : Natural := 0;
         modifiers     : Modifier_Set := (others => False);
         spattern      : Text;
      end record;

   type switches_fetch_cmd is
      record
         depends_also  : Boolean := False;
         avail_updates : Boolean := False;
         destination   : Text;
         name_patterns : Pkgtypes.Text_List.Vector;
      end record;

   type switches_remove_cmd is
      record
         force_breakage     : Boolean := False;
         inhibit_scripts    : Boolean := False;
         skip_verify        : Boolean := False;
         name_patterns      : Pkgtypes.Text_List.Vector;
      end record;

   type switches_autoremove_cmd is
      record
         inhibit_scripts    : Boolean := False;
         skip_verify        : Boolean := False;
      end record;

   type switches_check_cmd is
      record
         only_depends       : Boolean := False;
         only_files         : Boolean := False;
      end record;

   type switches_annotate_cmd is
      record
         tag                : Text;
         note               : Text;
         operation_set      : Boolean := False;
         operation_delete   : Boolean := False;
         operation_find     : Boolean := False;
         glob_input         : Boolean := False;
      end record;

   type audit_filter is (vulnerable, secure, none);
   type audit_level  is (summary, concise, full);
   type audit_format is (fmt_report, fmt_json, fmt_ucl);

   type switches_audit_cmd is
      record
         refresh            : Boolean      := False;
         filter             : audit_filter := vulnerable;
         level              : audit_level  := summary;
         format             : audit_format := fmt_report;
      end record;

   type pre_command_switches is
      record
         debug_setting      : A_Debug_Level := silent;
         version_setting    : version_depth := not_shown;
         list_commands      : Boolean := False;
         status_check       : Boolean := False;
         chroot_first       : Text;   -- mutually exclusive to install_rootdir
         install_rootdir    : Text;   -- mutually exclusive to chroot_first
         custom_configfile  : Text;
         custom_repos_dir   : Text;
         option_nvpairs     : Text;
         internet_protocol  : IP_support := no_restriction;
      end record;

   type Cldata is tagged
      record
         command                : Command_verb := cv_unset;
         help_command           : Command_verb := cv_unset;
         help_command2          : Command2_verb := cv2_unset;

         parse_error            : Boolean := False;
         error_message          : Text;
         next_argument          : Text;
         pending_argument       : Boolean := False;
         common_options         : switches_common;

         pre_command            : pre_command_switches;
         cmd_alias              : switches_alias_cmd;
         cmd_annotate           : switches_annotate_cmd;
         cmd_autoremove         : switches_autoremove_cmd;
         cmd_catalog            : switches_catalog_cmd;
         cmd_check              : switches_check_cmd;
         cmd_clean              : switches_clean_cmd;
         cmd_config             : switches_config_cmd;
         cmd_create             : switches_create_cmd;
         cmd_genrepo            : switches_genrepo_cmd;
         cmd_fetch              : switches_fetch_cmd;
         cmd_info               : switches_info_cmd;
         cmd_install            : switches_install_cmd;
         cmd_query              : switches_query_cmd;
         cmd_remove             : switches_remove_cmd;
         cmd_rquery             : switches_rquery_cmd;
         cmd_search             : switches_search_cmd;
         cmd_shell              : switches_shell_cmd;
         cmd_shlib              : switches_shlib_cmd;
         cmd_stats              : switches_stats_cmd;
         cmd_upgrade            : switches_upgrade_cmd;
         cmd_version            : switches_version_cmd;
         cmd_which              : switches_which_cmd;
         cmd_audit              : switches_audit_cmd;
      end record;

   --  Provide string equivalent to given command enumeration
   function convert_command_enum_to_label (command : Command_verb) return String;


end Raven.Cmd;
