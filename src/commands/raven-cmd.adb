--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Text_IO;

package body Raven.Cmd is

   -------------------------------------
   --  convert_command_enum_to_label  --
   -------------------------------------
   function convert_command_enum_to_label (command : Command_verb) return String
   is
   begin
      case command is
         when cv_unset      => return "";
         when cv_audit      => return "audit";
         when cv_alias      => return "alias";
         when cv_annotate   => return "annotate";
         when cv_autoremove => return "autoremove";
         when cv_catalog    => return "catalog";
         when cv_check      => return "check";
         when cv_clean      => return "clean";
         when cv_config     => return "config";
         when cv_create     => return "create";
         when cv_fetch      => return "fetch";
         when cv_info       => return "info";
         when cv_genrepo    => return "genrepo";
         when cv_help       => return "help";
         when cv_install    => return "install";
         when cv_query      => return "query";
         when cv_rquery     => return "rquery";
         when cv_remove     => return "remove";
         when cv_search     => return "search";
         when cv_shell      => return "shell";
         when cv_shlib      => return "shlib";
         when cv_stats      => return "stats";
         when cv_upgrade    => return "upgrade";
         when cv_version    => return "version";
         when cv_which      => return "which";
      end case;
   end convert_command_enum_to_label;

end Raven.Cmd;
