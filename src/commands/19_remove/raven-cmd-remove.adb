--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Event;
with Raven.Deinstall;
with Raven.Database.Remove;
with Raven.Database.Operations;
with Raven.Strings;

use Raven.Strings;

package body Raven.Cmd.Remove is

   package OPS renames Raven.Database.Operations;
   package DEL renames Raven.Database.Remove;


   ------------------------------
   --  execute_remove_command  --
   ------------------------------
   function execute_remove_command (comline : Cldata) return Boolean
   is
      success     : Boolean;
      rdb         : Database.RDB_Connection;
      toplist     : Pkgtypes.Package_Set.Vector;
      purge_list  : Pkgtypes.Package_Set.Vector;
      purge_order : Deinstall.Purge_Order_Crate.Vector;
   begin
      case OPS.rdb_open_localdb (rdb, Database.installed_packages) is
         when RESULT_OK => null;
         when others => return False;
      end case;

      if comline.common_options.all_installed_pkgs then
         success := DEL.top_level_deletion_list
           (db             => rdb,
            packages       => toplist,
            pattern        => "",
            all_packages   => True,
            override_exact => comline.common_options.exact_match,
            force          => comline.cmd_remove.force_breakage);
      else
         declare
            numpatt : constant Natural := Natural (comline.cmd_remove.name_patterns.Length);
         begin
            for patt_index in 0 .. numpatt - 1 loop
               if not DEL.top_level_deletion_list
                 (db             => rdb,
                  packages       => toplist,
                  pattern        => USS (comline.cmd_remove.name_patterns.Element (patt_index)),
                  all_packages   => False,
                  override_exact => comline.common_options.exact_match,
                  force          => comline.cmd_remove.force_breakage)
               then
                  success := False;
                  exit;
               end if;
            end loop;
         end;
      end if;

      if toplist.Is_Empty then
         if not comline.common_options.quiet then
            Event.emit_message ("No installed packages were selected for removal.");
            return True;
         end if;
      end if;

      DEL.recursive_removal
        (db           => rdb,
         top_packages => toplist,
         purge_list   => purge_list,
         force        => comline.cmd_remove.force_breakage);

      Deinstall.determine_purge_order (purge_list, purge_order);

      --  Show removal list unless --quiet is set
      Deinstall.show_proposed_queue
        (purge_list   => purge_list,
         purge_order  => purge_order,
         behave_quiet => comline.common_options.quiet,
         dryrun       => comline.common_options.dry_run);

      if comline.common_options.dry_run then
         return True;
      end if;

      if not Deinstall.granted_permission_to_proceed (comline.common_options.quiet) then
         return True;
      else
         if not comline.common_options.quiet then
            Event.emit_message ("");
         end if;
      end if;

      Deinstall.remove_packages_in_order
        (rdb          => rdb,
         purge_list   => purge_list,
         purge_order  => purge_order,
         skip_verify  => comline.cmd_remove.skip_verify,
         skip_scripts => comline.cmd_remove.inhibit_scripts,
         quiet        => comline.common_options.quiet);

      OPS.rdb_close (rdb);
      return success;
   end execute_remove_command;


end Raven.Cmd.Remove;
