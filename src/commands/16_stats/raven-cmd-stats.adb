--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Event;
with Raven.Metadata;
with Raven.Repository;
with Raven.Database.Lock;
with Raven.Database.Query;
with Raven.Database.Operations;
with Raven.Strings; use Raven.Strings;
with Archive.Unix;

package body Raven.Cmd.Stats is

   package LOK renames Raven.Database.Lock;
   package QRY renames Raven.Database.Query;
   package OPS renames Raven.Database.Operations;

   -----------------------------
   --  execute_stats_command  --
   -----------------------------
   function execute_stats_command (comline : Cldata) return Boolean
   is
      show_catalog : Boolean := True;
      show_localdb : Boolean := True;
   begin
      if comline.cmd_stats.local_only then
         show_catalog := False;
      end if;

      if comline.cmd_stats.catalog_only then
         show_localdb := False;
      end if;

      if show_catalog then
         if not show_catalog_stats (single_repo  => USS (comline.common_options.repo_name),
                                    option_quiet => comline.common_options.quiet)
         then
            return False;
         end if;
      end if;

      if show_localdb then
         if not show_installation_stat then
            return False;
         end if;
      end if;

      return True;
   end execute_stats_command;


   --------------------------
   --  show_catalog_stats  --
   --------------------------
   function show_catalog_stats (single_repo : String; option_quiet : Boolean) return Boolean
   is
      mirrors : Repository.A_Repo_Config_Set;
      rdb     : Database.RDB_Connection;
      LF      : constant Character := Character'Val (10);
   begin
      if Archive.Unix.user_is_root then
         Repository.load_repository_configurations (mirrors, single_repo);
         if not Repository.create_local_catalog_database
           (remote_repositories  => mirrors,
            forced               => False,
            quiet                => option_quiet)
         then
            Event.emit_error ("Failed to update the local catalog");
         end if;
      end if;

      if not OPS.localdb_exists (Database.catalog) then
         Event.emit_error
           ("Catalog database is missing, should be here: " & OPS.localdb_path (Database.catalog));
         return False;
      end if;

      case OPS.rdb_open_localdb (rdb, Database.catalog) is
         when RESULT_OK => null;
         when others => return False;
      end case;

      if not LOK.obtain_lock (rdb, LOK.lock_readonly) then
         Event.emit_error (LOK.no_read_lock);
         OPS.rdb_close (rdb);
         return False;
      end if;

      declare
         measurements : QRY.A_Measurement_Set;
      begin
         QRY.package_measurement (rdb, measurements);
         Event.emit_message (LF & "Repository catalog availability:");
         Event.emit_message ("  Number of packages      : " & int2str (measurements.num_packages));
         Event.emit_message ("  Number of variants      : " & int2str (measurements.num_variants));
         Event.emit_message ("  Number of ports         : " & int2str (measurements.num_ports));
         Event.emit_message ("  Combined package size   : " &
                               Metadata.human_readable_size (int64 (measurements.sum_pkgsize)));
         Event.emit_message ("  Combined extracted size : " &
                               Metadata.human_readable_size (int64 (measurements.sum_flatsize)));
      end;

      if not LOK.release_lock (rdb, LOK.lock_readonly) then
         Event.emit_error (LOK.no_read_unlock);
         OPS.rdb_close (rdb);
         return False;
      end if;

      OPS.rdb_close (rdb);
      return True;

   end show_catalog_stats;


   ------------------------------
   --  show_installation_stat  --
   ------------------------------
   function show_installation_stat return Boolean
   is
      ldb : Database.RDB_Connection;
      LF  : constant Character := Character'Val (10);
   begin
      if not OPS.localdb_exists (Database.installed_packages) then
         Event.emit_error ("Local database is missing, should be here: " &
                             OPS.localdb_path (Database.installed_packages));
         return False;
      end if;

      case OPS.rdb_open_localdb (ldb, Database.installed_packages) is
         when RESULT_OK => null;
         when others => return False;
      end case;

      if not LOK.obtain_lock (ldb, LOK.lock_readonly) then
         Event.emit_error (LOK.no_read_lock);
         OPS.rdb_close (ldb);
         return False;
      end if;

      declare
         measurements : QRY.A_Measurement_Set;
      begin
         QRY.package_measurement (ldb, measurements);
         Event.emit_message (LF & "Locally installed packages:");
         Event.emit_message ("  Installed packages      : " & int2str (measurements.num_packages));
         Event.emit_message ("  Variant subset          : " & int2str (measurements.num_variants));
         Event.emit_message ("  Port subset             : " & int2str (measurements.num_ports));
         Event.emit_message ("  Disk space occupied     :" &
                               measurements.sum_flatsize'Img & " bytes");
         Event.emit_message ("  Disk space occupied     : " &
                               Metadata.human_readable_size (int64 (measurements.sum_flatsize)));
      end;

      if not LOK.release_lock (ldb, LOK.lock_readonly) then
         Event.emit_error (LOK.no_read_unlock);
         OPS.rdb_close (ldb);
         return False;
      end if;

      OPS.rdb_close (ldb);
      return True;

   end show_installation_stat;


end Raven.Cmd.Stats;
