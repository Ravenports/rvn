--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Event;
with Raven.Deinstall;
with Raven.Database.Remove;
with Raven.Database.Operations;

package body Raven.Cmd.Autoremove is

   package DEL renames Raven.Database.Remove;
   package OPS renames Raven.Database.Operations;

   ----------------------------------
   --  execute_autoremove_command  --
   ----------------------------------
   function execute_autoremove_command (comline : Cldata) return Boolean
   is
      rdb         : Database.RDB_Connection;
      toplist     : Pkgtypes.Package_Set.Vector;
      purge_list  : Pkgtypes.Package_Set.Vector;
      purge_order : Deinstall.Purge_Order_Crate.Vector;
   begin
      case OPS.rdb_open_localdb (rdb, Database.installed_packages) is
         when RESULT_OK => null;
         when others => return False;
      end case;

      if not DEL.autoremoval_list (rdb, toplist) then
         return False;
      end if;

      if toplist.Is_Empty then
         if not comline.common_options.quiet then
            Event.emit_message ("No installed packages were selected for autoremoval.");
            return True;
         end if;
      end if;

      DEL.prune_candidates_with_reverse_deps (rdb, toplist, purge_list);
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

      --  Run query again to see if any new packages were orphaned and let the user know.
      if not comline.common_options.quiet then
         declare
            toplist2    : Pkgtypes.Package_Set.Vector;
            purge_list2 : Pkgtypes.Package_Set.Vector;
         begin
            if DEL.autoremoval_list (rdb, toplist2) then
               if not toplist2.Is_Empty then
                  DEL.prune_candidates_with_reverse_deps (rdb, toplist2, purge_list2);
                  if not purge_list2.Is_Empty then
                     Event.emit_message ("----------------------------------------------");
                     Event.emit_message ("  New orphan packages have been detected.");
                     Event.emit_message ("You may wish to run '" & progname &
                                           " autoremove' again.");
                     Event.emit_message ("----------------------------------------------");
                  end if;
               end if;
            end if;
         end;
      end if;

      OPS.rdb_close (rdb);
      return True;

   end execute_autoremove_command;

end Raven.Cmd.Autoremove;
