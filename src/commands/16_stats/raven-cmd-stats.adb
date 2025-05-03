--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Event;
with Raven.Context;
with Raven.Metadata;
with Raven.Repository;
with Raven.Database.Lock;
with Raven.Database.Query;
with Raven.Database.Operations;
with Raven.Strings; use Raven.Strings;
with Archive.Unix;
with Archive.Dirent.Scan;

package body Raven.Cmd.Stats is

   package LOK renames Raven.Database.Lock;
   package QRY renames Raven.Database.Query;
   package OPS renames Raven.Database.Operations;
   package SCN renames Archive.Dirent.Scan;

   -----------------------------
   --  execute_stats_command  --
   -----------------------------
   function execute_stats_command (comline : Cldata) return Boolean
   is
      show_catalog : Boolean := comline.cmd_stats.local_only;
      show_localdb : Boolean := comline.cmd_stats.catalog_only;
      show_cached  : Boolean := comline.cmd_stats.cache_only;
   begin
      if not show_catalog and then
        not show_localdb and then
        not show_cached
      then
         show_catalog := True;
         show_localdb := True;
         show_cached := True;
      end if;

      if show_catalog then
         if not show_catalog_stats (single_repo  => USS (comline.common_options.repo_name),
                                    option_quiet => comline.common_options.quiet)
         then
            return False;
         end if;
      end if;

      if show_localdb then
         if not show_installation_stats then
            return False;
         end if;
      end if;

      if show_cached then
         show_cache_stats;
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


   -------------------------------
   --  show_installation_stats  --
   -------------------------------
   function show_installation_stats return Boolean
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

   end show_installation_stats;


   ------------------------
   --  show_cache_stats  --
   ------------------------
   procedure show_cache_stats
   is
      cachedir       : constant String := Context.reveal_cache_directory;
      LF             : constant Character := Character'Val (10);
      cache_contents : SCN.dscan_crate.Vector;
      num_current    : Natural := 0;
      num_obsolete   : Natural := 0;
      size_current   : int64 := 0;
      size_obsolete  : int64 := 0;
      files_tallied  : Pkgtypes.Text_List.Vector;

      procedure tally_symlink_targets (Position : SCN.dscan_crate.Cursor)
      is
         path : constant String := Archive.Dirent.full_path (SCN.dscan_crate.Element (Position));
         fileattr : Archive.Unix.File_Characteristics;
      begin
         fileattr := Archive.Unix.get_charactistics (path);
         case fileattr.ftype is
            when Archive.symlink => null;
            when others => return;
         end case;
         declare
            target : constant String := Archive.Unix.link_target (path);
         begin
            fileattr := Archive.Unix.get_charactistics (target);
            case fileattr.ftype is
               when Archive.regular => null;
               when others => return;
            end case;
            num_current := num_current + 1;
            size_current := size_current + int64 (fileattr.size);
            files_tallied.Append (Strings.SUS (target));
         end;
      end tally_symlink_targets;

      procedure tally_orphans (Position : SCN.dscan_crate.Cursor)
      is
         path : constant String := Archive.Dirent.full_path (SCN.dscan_crate.Element (Position));
         fileattr : Archive.Unix.File_Characteristics;
      begin
         fileattr := Archive.Unix.get_charactistics (path);
         case fileattr.ftype is
            when Archive.regular => null;
            when others => return;
         end case;
         if not files_tallied.Contains (Strings.SUS (path)) then
            num_obsolete := num_obsolete + 1;
            size_obsolete := size_obsolete + int64 (fileattr.size);
         end if;
      end tally_orphans;
   begin
      SCN.scan_directory (cachedir, cache_contents);
      cache_contents.Iterate (tally_symlink_targets'Access);
      cache_contents.Iterate (tally_orphans'Access);
      Event.emit_message (LF & "Cache of downloaded packages:");
      Event.emit_message ("  Current packages        : " & int2str (num_current));
      Event.emit_message ("  Disk space occupied     : " &
                            Metadata.human_readable_size (size_current));
      Event.emit_message ("  Obsolete packages       : " & int2str (num_obsolete));
      Event.emit_message ("  Disk space occupied     : " &
                            Metadata.human_readable_size (size_obsolete));
   end show_cache_stats;

end Raven.Cmd.Stats;
