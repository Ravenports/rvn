--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Event;
with Raven.Deinstall;
with Raven.Database.Lock;
with Raven.Database.Remove;
with Raven.Database.Operations;
with Raven.Strings;

use Raven.Strings;

package body Raven.Cmd.Remove is

   package OPS renames Raven.Database.Operations;
   package DEL renames Raven.Database.Remove;
   package LOK renames Raven.Database.Lock;


   ------------------------------
   --  execute_remove_command  --
   ------------------------------
   function execute_remove_command (comline : Cldata) return Boolean
   is
      success     : Boolean := True;
      rdb         : Database.RDB_Connection;
      toplist     : Pkgtypes.Package_Set.Vector;
      purge_list  : Pkgtypes.Package_Set.Vector;
      purge_order : Deinstall.Purge_Order_Crate.Vector;
      active_lock : LOK.lock_type := LOK.lock_readonly;

      procedure release_active_lock is
      begin
         if not LOK.release_lock (rdb, active_lock) then
            case active_lock is
               when LOK.lock_advisory  => Event.emit_error (LOK.no_advisory_unlock);
               when LOK.lock_exclusive => Event.emit_error (LOK.no_exclusive_unlock);
               when LOK.lock_readonly  => Event.emit_error (LOK.no_read_unlock);
            end case;
         end if;
      end release_active_lock;

      procedure exit_lock is
      begin
         release_active_lock;
         OPS.rdb_close (rdb);
      end exit_lock;

      function activate_lock return Boolean is
      begin
         if not LOK.obtain_lock (rdb, active_lock) then
            case active_lock is
               when LOK.lock_advisory  => Event.emit_error (LOK.no_adv_lock);
               when LOK.lock_exclusive => Event.emit_error (LOK.no_exc_lock);
               when LOK.lock_readonly  => Event.emit_error (LOK.no_read_lock);
            end case;
            OPS.rdb_close (rdb);
            return False;
         end if;
         return True;
      end activate_lock;
   begin
      case OPS.rdb_open_localdb (rdb, Database.installed_packages) is
         when RESULT_OK => null;
         when others => return False;
      end case;

      if not activate_lock then
         return False;
      end if;

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

      if not success then
         exit_lock;
         return False;
      end if;

      if toplist.Is_Empty then
         if not comline.common_options.quiet then
            Event.emit_message ("No installed packages were selected for removal.");
            exit_lock;
            return True;
         end if;
      end if;

      DEL.recursive_removal
        (db           => rdb,
         top_packages => toplist,
         purge_list   => purge_list,
         force        => comline.cmd_remove.force_breakage);

      release_active_lock;
      Deinstall.determine_purge_order (purge_list, purge_order);

      --  Show removal list unless --quiet is set
      Deinstall.show_proposed_queue
        (purge_list   => purge_list,
         purge_order  => purge_order,
         behave_quiet => comline.common_options.quiet,
         dryrun       => comline.common_options.dry_run);

      if comline.common_options.dry_run then
         OPS.rdb_close (rdb);
         return True;
      end if;

      if not Deinstall.granted_permission_to_proceed then
         OPS.rdb_close (rdb);
         return True;
      else
         if not comline.common_options.quiet then
            Event.emit_message ("");
         end if;
      end if;

      active_lock := LOK.lock_advisory;
      if not activate_lock then
         return False;
      end if;

      Deinstall.remove_packages_in_order
        (rdb          => rdb,
         purge_list   => purge_list,
         purge_order  => purge_order,
         skip_verify  => comline.cmd_remove.skip_verify,
         skip_scripts => comline.cmd_remove.inhibit_scripts,
         quiet        => comline.common_options.quiet);

      exit_lock;
      return success;
   end execute_remove_command;


end Raven.Cmd.Remove;
