--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Event;
with Raven.Context;
with Raven.Cmd.Unset;
with Raven.Repository;
with Raven.Database.Fetch;
with Raven.Database.Operations;
with Raven.Strings; use Raven.Strings;
with Archive.Unix;

package body Raven.Cmd.Fetch is

   package RCU renames Raven.Cmd.Unset;
   package FET renames Raven.Database.Fetch;
   package OPS renames Raven.Database.Operations;

   -----------------------------
   --  execute_fetch_command  --
   -----------------------------
   function execute_fetch_command (comline : Cldata) return Boolean
   is
      rdb        : Database.RDB_Connection;
      behave_cs  : Boolean := comline.common_options.case_sensitive;
      behave_yes : Boolean := comline.common_options.assume_yes;
      success    : Boolean;
      repo_solo  : constant String := USS (comline.common_options.repo_name);
   begin
      if not refresh_catalog (repo_solo) then
         return False;
      end if;

      case OPS.rdb_open_localdb (rdb, Database.catalog) is
         when RESULT_OK => null;
         when others => return False;
      end case;

      if Context.reveal_case_sensitive then
         behave_cs := True;
      end if;

      if RCU.config_setting (RCU.CFG.assume_yes) then
         behave_yes := True;
      end if;

      if comline.cmd_fetch.avail_updates then
         declare
            upgrade_list : Pkgtypes.Text_List.Vector;
         begin
            declare
               ldb : Database.RDB_Connection;
            begin
               case OPS.rdb_open_localdb (ldb, Database.installed_packages) is
                  when RESULT_OK => null;
                  when others => return False;
               end case;
               FET.list_of_upgradeable_packages (ldb, rdb, upgrade_list);
               OPS.rdb_close (ldb);
            end;
            if upgrade_list.Is_Empty then
               Event.emit_message ("All installed packages are at the latest version.");
               return True;
            end if;

            success := FET.rvn_core_retrieval
              (db           => rdb,
               patterns     => upgrade_list,
               behave_exact => True,
               behave_cs    => False,
               behave_quiet => comline.common_options.quiet,
               behave_yes   => behave_yes,
               select_all   => False,
               select_deps  => False,
               destination  => USS (comline.cmd_fetch.destination),
               single_repo  => repo_solo);
         end;
      else
         success := FET.rvn_core_retrieval
           (db           => rdb,
            patterns     => comline.cmd_fetch.name_patterns,
            behave_exact => comline.common_options.exact_match,
            behave_cs    => behave_cs,
            behave_quiet => comline.common_options.quiet,
            behave_yes   => behave_yes,
            select_all   => comline.common_options.all_installed_pkgs,
            select_deps  => comline.cmd_fetch.depends_also,
            destination  => USS (comline.cmd_fetch.destination),
            single_repo  => repo_solo);
      end if;

      OPS.rdb_close (rdb);
      return (success);

   end execute_fetch_command;


   -----------------------
   --  refresh_catalog  --
   -----------------------
   function refresh_catalog (single_repo : String) return Boolean
   is
      mirrors : Repository.A_Repo_Config_Set;
   begin
      if Archive.Unix.user_is_root then
         Repository.load_repository_configurations (mirrors, single_repo);
         if not Repository.create_local_catalog_database
           (remote_repositories  => mirrors,
            forced               => False,
            quiet                => True)
         then
            Event.emit_error ("Failed to update the local catalog");
         end if;
      end if;

      if not OPS.localdb_exists (Database.catalog) then
         Event.emit_error
           ("Catalog database is missing, should be here: " & OPS.localdb_path (Database.catalog));
         return False;
      end if;

      return True;
   end refresh_catalog;

end Raven.Cmd.Fetch;
