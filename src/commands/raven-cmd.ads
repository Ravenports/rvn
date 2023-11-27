--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt


with Ada.Text_IO;
with Ada.Containers.Vectors;

package Raven.Cmd is

   package TIO renames Ada.Text_IO;
   package CON renames Ada.Containers;

   type Cldata is private;

private

   package string_crate is new CON.Vectors
     (Element_Type => Text,
      Index_Type   => Natural,
      "="          => SU."=");

   type Command_verb is
     (cv_unset,
      cv_create,
      cv_info,
      cv_help
     );

     --   cv_add,
     --   cv_alias,
     --   cv_annotate,
     --   cv_autoremove,
     --   cv_check,
     --   cv_clean,
     --   cv_config,
     --   cv_delete,
     --   cv_fetch,
     --   ,
     --   cv_install,
     --   cv_lock,
     --   cv_query,
     --   cv_remove,
     --   cv_repo,
     --   cv_rquery,
     --   cv_search,
     --   cv_set,
     --   cv_shell,
     --   cv_shlib,
     --   cv_ssh,
     --   cv_stats,
     --   cv_unlock,
     --   cv_update,
     --   cv_upgrade,
     --   cv_version,
     --   cv_which

   type Command2_verb is
     (cv2_unset,
      cv2_main,
      cv2_repository,
      cv2_main_conf
     );

   type version_depth is (not_shown, just_version, dump_configuration);
   subtype ST_Version is Natural range 0 .. 2;

   type switches_create_cmd is
      record
         output_dir      : Text;
         metadata_file   : Text;
         whitelist_file  : Text;
         timestamp       : Text;
         prefix          : Text;
      end record;

   type switches_common is
      record
         verbose            : Boolean := False;
         quiet              : Boolean := False;  --  -q, --quiet
         case_sensitive     : Boolean := False;  --  -C, --case-sensitive
         case_insensitive   : Boolean := False;  --  -i, --case-insensitive (default)
         shell_glob         : Boolean := False;  --  -g, --glob
         regex              : Boolean := False;  --  -x, --regex
         dry_run            : Boolean := False;  --  -n, --dry-run
         assume_yes         : Boolean := False;  --  -y, --yes
         all_installed_pkgs : Boolean := False;  --  -a, --all
         name_pattern       : Text;
      end record;

   type switches_info_cmd is
      record
         annotations        : Boolean := False;  --  -A, --annotations
         shlibs_provided    : Boolean := False;  --  -B, --required-shlibs
         shlibs_used        : Boolean := False;  --  -b, --provided-shlibs
         install_message    : Boolean := False;  --  -M, --message
         description        : Boolean := False;  --  -D, --description
         dependencies       : Boolean := False;  --  -d, --dependencies (package depends on them)
         installed          : Boolean := False;  --  -e, --exists
         full_information   : Boolean := False;  --  -f, --full
         comment            : Boolean := False;  --  -I, --comment
         list_files         : Boolean := False;  --  -l, --list-files
         install_prefix     : Boolean := False;  --  -p, --prefix
         raw_manifest       : Boolean := False;  --  -R, --raw
         rev_deps           : Boolean := False;  --  -r, --required-by (list pkgs that require it)
         total_size         : Boolean := False;  --  -s, --size
         namebase           : Boolean := False;  --  -N, --namebase
         subpackage         : Boolean := False;  --  -S, --subpackage
         variant            : Boolean := False;  --  -V, --variant
         path_archive_file  : Text;
      end record;

   type Cldata is
      record
         global_debug           : A_Debug_Level := silent;
         global_chroot          : Text;
         global_config_file     : Text;
         global_repo_config_dir : Text;
         global_root_dir        : Text;
         global_options         : Text;

         unset_version          : version_depth := not_shown;
         unset_list_cmd         : Boolean := False;
         unset_status_check     : Boolean := False;

         command                : Command_verb := cv_unset;
         parse_error            : Boolean := False;
         common_options         : switches_common;

         help_command           : Command_verb := cv_unset;
         help_command2          : Command2_verb := cv2_unset;

         cmd_create             : switches_create_cmd;
         cmd_info               : switches_info_cmd;

         error_message          : Text;
      end record;

   --  Provide string equivalent to given command enumeration
   function convert_command_enum_to_label (command : Command_verb) return String;


end Raven.Cmd;
