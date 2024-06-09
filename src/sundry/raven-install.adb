--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Characters.Latin_1;
with Raven.Event;
with Raven.Context;
with Raven.Version;
with Raven.Metadata;
with Raven.Cmd.Unset;
with Raven.Deinstall;
with Raven.Miscellaneous;
with Raven.Database.Add;
with Raven.Database.Pkgs;
with Raven.Database.Lock;
with Raven.Database.Fetch;
with Raven.Database.Operations;
with Raven.Strings;
with Archive.Unpack;
with Archive.Unix;
with ThickUCL;

use Raven.Strings;


package body Raven.Install is

   package TIO  renames Ada.Text_IO;
   package LAT  renames Ada.Characters.Latin_1;
   package MISC renames Raven.Miscellaneous;
   package INST renames Raven.Database.Add;
   package PKGS renames Raven.Database.Pkgs;
   package LOK  renames Raven.Database.Lock;
   package FET  renames Raven.Database.Fetch;
   package OPS  renames Raven.Database.Operations;
   package RCU  renames Raven.Cmd.Unset;

   function reinstall_or_upgrade (rdb         : in out Database.RDB_Connection;
                                  action      : refresh_action;
                                  current_pkg : Pkgtypes.A_Package;
                                  updated_pkg : String;
                                  rootdir     : String;
                                  no_scripts  : Boolean;
                                  post_report : TIO.File_Type) return Boolean
   is
      features  : Archive.Unix.File_Characteristics;
      file_list : Archive.Unpack.file_records.Vector;
      operation : Archive.Unpack.Darc;
      metatree  : ThickUCL.UclTree;
      shiny_pkg : Pkgtypes.A_Package;
      success   : Boolean;

      function extract_location return String is
      begin
         if rootdir = "" then
            return "/";
         end if;
         return rootdir;
      end extract_location;

      procedure transfer_custom_notes (Position : Pkgtypes.NoteSet.Cursor)
      is
         mynote : Pkgtypes.Note_Item renames Pkgtypes.NoteSet.Element (Position);
      begin
         if mynote.custom then
            shiny_pkg.annotations.Insert (mynote.tag, mynote);
         end if;
      end transfer_custom_notes;
   begin
      features := Archive.Unix.get_charactistics (updated_pkg);
      case features.ftype is
         when Archive.regular => null;
         when Archive.unsupported =>
            Event.emit_error ("Dev error: " & updated_pkg & " package D.N.E.");
            return False;
         when others =>
            Event.emit_error ("Not a regular file: " & updated_pkg);
            return False;
      end case;

      operation.open_rvn_archive (updated_pkg, Archive.silent, Archive.Unix.not_connected);
      if not operation.extract_manifest (file_list, extract_location) then
         Event.emit_error ("Failed to extract manifest of packaged files.");
      end if;
      operation.populate_metadata_tree (metatree);
      operation.close_rvn_archive;

      Metadata.convert_to_package (metatree, file_list, shiny_pkg, current_pkg.automatic);
      current_pkg.annotations.Iterate (transfer_custom_notes'Access);

      case action is
         when upgrade =>
            Deinstall.deinstall_extracted_package (installed_package   => current_pkg,
                                                   verify_digest_first => False,
                                                   quiet               => True,
                                                   inhibit_scripts     => no_scripts,
                                                   post_report         => post_report);

            success := PKGS.rdb_register_package (db     => rdb,
                                                  pkg    => shiny_pkg,
                                                  forced => False);
            if success then
               success := install_files_from_archive (archive_path    => updated_pkg,
                                                      root_directory  => extract_location,
                                                      inhibit_scripts => no_scripts,
                                                      be_silent       => True,
                                                      dry_run_only    => False,
                                                      upgrading       => True,
                                                      package_data    => shiny_pkg);
            end if;
         when reinstall =>
            success := PKGS.rdb_register_package (db     => rdb,
                                                  pkg    => shiny_pkg,
                                                  forced => True);
            if success then
               success := install_files_from_archive (archive_path    => updated_pkg,
                                                      root_directory  => extract_location,
                                                      inhibit_scripts => no_scripts,
                                                      be_silent       => True,
                                                      dry_run_only    => False,
                                                      upgrading       => False,
                                                      package_data    => shiny_pkg);
               end if;

         when new_install =>
            success := False;
      end case;

      return success;

   end reinstall_or_upgrade;


   ----------------------------------
   --  install_files_from_archive  --
   ----------------------------------
   function install_files_from_archive
     (archive_path      : String;
      root_directory    : String;
      inhibit_scripts   : Boolean;
      be_silent         : Boolean;
      dry_run_only      : Boolean;
      upgrading         : Boolean;
      package_data      : Pkgtypes.A_Package) return Boolean
   is
      operation : Archive.Unpack.Darc;
      level     : Archive.info_level := Archive.normal;
      basename  : constant String := MISC.archive_basename (archive_path);
      rootuser  : constant Boolean := Archive.Unix.user_is_root;
      pipe_fd   : constant Archive.Unix.File_Descriptor :=
        Archive.Unix.File_Descriptor (Context.reveal_event_pipe);
      good_extraction : Boolean;

      function action return String is
      begin
         if upgrading then
            return "upgrade";
         end if;
         return "install";
      end action;
   begin
      if dry_run_only then
         Event.emit_message ("dry-run: " & action & " " & basename & " package");
         return True;
      else
         if not be_silent then
            Event.emit_notice (action & " " & basename & " package");
         end if;
      end if;

      if be_silent then
         level := Archive.silent;
      end if;

      Event.emit_extract_begin (package_data);
      operation.open_rvn_archive (archive_path, level, pipe_fd);
      good_extraction := operation.extract_archive
        (top_directory => root_directory,
         set_owners    => rootuser,
         set_perms     => rootuser,
         set_modtime   => False,
         skip_scripts  => inhibit_scripts,
         upgrading     => upgrading);
      operation.close_rvn_archive;
      Event.emit_extract_end (package_data);

      return good_extraction;
   end install_files_from_archive;


   -------------------------------
   --  install_remote_packages  --
   -------------------------------
   function install_remote_packages (opt_exact_match  : Boolean;
                                     opt_quiet        : Boolean;
                                     opt_automatic    : Boolean;
                                     opt_manual       : Boolean;
                                     opt_drop_depends : Boolean;
                                     opt_force        : Boolean;
                                     opt_skip_scripts : Boolean;
                                     opt_dry_run      : Boolean;
                                     opt_fetch_only   : Boolean;
                                     single_repo      : String;
                                     patterns         : Pkgtypes.Text_List.Vector)
                                     return Boolean
   is
      --  case-sensitive accessed via RCU settings
      --  assume-yes accessed via RCU settings

      rdb         : Database.RDB_Connection;
      localdb     : Database.RDB_Connection;
      active_lock : LOK.lock_type := LOK.lock_advisory;
      succeeded   : Boolean;
      released1   : Boolean;
      released2   : Boolean;
      catalog_map : Pkgtypes.Package_Map.Map;
      install_map : Pkgtypes.Package_Map.Map;
      queue       : Install_Order_Set.Vector;

      function release_active_lock (db : in out Database.RDB_Connection) return Boolean is
      begin
         if not LOK.release_lock (db, active_lock) then
            case active_lock is
               when LOK.lock_advisory  => Event.emit_error (LOK.no_advisory_unlock);
               when LOK.lock_exclusive => Event.emit_error (LOK.no_exclusive_unlock);
               when LOK.lock_readonly  => Event.emit_error (LOK.no_read_unlock);
            end case;
            return False;
         end if;
         return True;
      end release_active_lock;
   begin
      if opt_dry_run then
         active_lock := LOK.lock_readonly;
      end if;

      case OPS.rdb_open_localdb (rdb, Database.catalog) is
         when RESULT_OK => null;
         when others => return False;
      end case;

      case OPS.rdb_open_localdb (localdb, Database.installed_packages) is
         when RESULT_OK => null;
         when others =>
            OPS.rdb_close (localdb);
            return False;
      end case;

      if not LOK.obtain_lock (rdb, active_lock) then
         case active_lock is
            when LOK.lock_advisory  => Event.emit_error (LOK.no_adv_lock);
            when LOK.lock_exclusive => Event.emit_error (LOK.no_exc_lock);
            when LOK.lock_readonly  => Event.emit_error (LOK.no_read_lock);
         end case;
         OPS.rdb_close (rdb);
         OPS.rdb_close (localdb);
         return False;
      end if;

      if not LOK.obtain_lock (localdb, active_lock) then
         case active_lock is
            when LOK.lock_advisory  => Event.emit_error (LOK.no_adv_lock);
            when LOK.lock_exclusive => Event.emit_error (LOK.no_exc_lock);
            when LOK.lock_readonly  => Event.emit_error (LOK.no_read_lock);
         end case;
         released1 := release_active_lock (rdb);
         OPS.rdb_close (rdb);
         OPS.rdb_close (localdb);
         return False;
      end if;

      succeeded := assemble_work_queue (rdb, opt_exact_match, patterns, catalog_map);
      if succeeded then
         declare
            priority    : Descendant_Set.Vector;
            cache_map   : Pkgtypes.Package_Map.Map;
            install_map : Pkgtypes.Package_Map.Map;
            upgrades    : Pkgtypes.Text_List.Vector;
            fetch_list  : Pkgtypes.Text_List.Vector;
            file_collection : Pkgtypes.NV_Pairs.Map;

            procedure gather_upgrades (Position : Install_Order_Set.Cursor)
            is
               myrec : Install_Order_Type renames Install_Order_Set.Element (Position);
            begin
               case myrec.action is
                  when new_install | reinstall => null;
                  when upgrade => upgrades.Append (myrec.nsv);
               end case;
            end gather_upgrades;

            procedure gather_fetch_list (Position : Install_Order_Set.Cursor)
            is
            begin
               fetch_list.Append (Install_Order_Set.Element (Position).nsv);
            end gather_fetch_list;

         begin
            calculate_descendants (rdb, catalog_map, cache_map, priority);
            load_installation_data (localdb, cache_map, install_map);
            finalize_work_queue
              (localdb       => localdb,
               install_map   => install_map,
               cache_map     => cache_map,
               priority      => priority,
               opt_automatic => opt_automatic,
               opt_manual    => opt_manual,
               opt_exactly   => opt_exact_match,
               opt_force     => opt_force,
               opt_drop_deps => opt_drop_depends,
               queue         => queue);

            --  if queue is empty, there's nothing more to do
            if not queue.Is_Empty then
               queue.Iterate (gather_upgrades'Access);
               queue.Iterate (gather_fetch_list'Access);

               INST.collect_installed_files (localdb, upgrades, file_collection);

               succeeded := FET.rvn_core_retrieval (db           => rdb,
                                                    patterns     => fetch_list,
                                                    behave_exact => True,
                                                    behave_cs    => False,
                                                    behave_quiet => opt_quiet,
                                                    select_all   => False,
                                                    select_deps  => False,
                                                    destination  => "",
                                                    single_repo  => single_repo);

               if succeeded then
                  succeeded := conflict_free (queue, cache_map, file_collection);
               end if;

               if succeeded then
                  show_proposed_queue (queue, cache_map, install_map, opt_quiet);
                  succeeded := granted_permission_to_proceed (opt_quiet);
               end if;
            end if;
         end;
      end if;

      released2 := release_active_lock (localdb);
      OPS.rdb_close (localdb);

      released1 := release_active_lock (rdb);
      OPS.rdb_close (rdb);

      return succeeded and then released1 and then released2;

   end install_remote_packages;


   ---------------------------
   --  assemble_work_queue  --
   ---------------------------
   function assemble_work_queue (rdb             : in out Database.RDB_Connection;
                                 opt_exact_match : Boolean;
                                 patterns        : Pkgtypes.Text_List.Vector;
                                 toplevel        : in out Pkgtypes.Package_Map.Map) return Boolean
   is
      success   : Boolean := True;
      numpatt   : constant Natural := Natural (patterns.Length);

      procedure merge (Position : Pkgtypes.Package_Set.Cursor)
      is
         myrec : Pkgtypes.A_Package := Pkgtypes.Package_Set.Element (Position);
         nsvv : constant String := Pkgtypes.nsv_identifier (myrec);
         nsvv_key : constant Text := SUS (nsvv);
      begin
         if not toplevel.Contains (nsvv_key) then
            toplevel.Insert (nsvv_key, myrec);
         end if;
      end merge;
   begin

      toplevel.Clear;
      for patt_index in 0 .. numpatt - 1 loop
         declare
            small_set : Pkgtypes.Package_Set.Vector;
         begin
            success := INST.top_level_addition_list
              (db             => rdb,
               packages       => small_set,
               pattern        => USS (patterns.Element (patt_index)),
               override_exact => opt_exact_match);
            exit when not success;
            small_set.Iterate (merge'Access);
         end;
      end loop;

      return success;
   end assemble_work_queue;


   -----------------------------
   --  calculate_descendants  --
   -----------------------------
   procedure calculate_descendants
     (rdb         : in out Database.RDB_Connection;
      catalog_map : Pkgtypes.Package_Map.Map;
      cache_map   : in out Pkgtypes.Package_Map.Map;
      priority    : in out Descendant_Set.Vector)
   is
      procedure calc (Position : Pkgtypes.Package_Map.Cursor)
      is
         catpkg : Pkgtypes.A_Package renames Pkgtypes.Package_Map.Element (Position);
         myrec : Descendant_Type;

         procedure check_single_dep (innerpos : Pkgtypes.NV_Pairs.Cursor)
         is
            dep_nsv : Text renames Pkgtypes.NV_Pairs.Key (innerpos);
            local_set : Pkgtypes.Package_Set.Vector;
         begin
            myrec.descendents := myrec.descendents + 1;
            if cache_map.Contains (dep_nsv) then
               Event.emit_debug (moderate, "cached " & USS (dep_nsv));
               cache_map.Element (dep_nsv).dependencies.Iterate (check_single_dep'Access);
            else
               if INST.top_level_addition_list
                 (db             => rdb,
                  packages       => local_set,
                  pattern        => USS (dep_nsv),
                  override_exact => True)
               then
                  if not local_set.Is_Empty then
                     declare
                        new_rec : Pkgtypes.A_Package := local_set.Element (0);
                     begin
                        cache_map.Insert (dep_nsv, new_rec);
                        cache_map.Element (dep_nsv).dependencies.Iterate (check_single_dep'Access);
                     end;
                  end if;
               end if;
            end if;
         end check_single_dep;

      begin
         myrec.nsv := SUS (Pkgtypes.nsv_identifier (catpkg));
         myrec.descendents := 1;
         catpkg.dependencies.Iterate (check_single_dep'Access);
         Event.emit_debug (moderate, USS (myrec.nsv) & " descendents=" & myrec.descendents'Img);
         priority.Append (myrec);
      end calc;

      procedure clone (Position : Pkgtypes.Package_Map.Cursor)
      is
         myrec : Pkgtypes.A_Package := Pkgtypes.Package_Map.Element (Position);
      begin
         cache_map.Insert (Pkgtypes.Package_Map.Key (Position), myrec);
      end clone;
   begin
      priority.Clear;
      cache_map.Clear;
      catalog_map.Iterate (clone'Access);
      catalog_map.Iterate (calc'Access);
      Desc_sorter.Sort (priority);
   end calculate_descendants;


   -----------------------------------
   --  calc_dependency_descendants  --
   -----------------------------------
   procedure calc_dependency_descendants
     (depend_set : Pkgtypes.NV_Pairs.Map;
      cache_map  : Pkgtypes.Package_Map.Map;
      priority   : in out Descendant_Set.Vector)
   is
      procedure calc (Position : Pkgtypes.NV_Pairs.Cursor)
      is
         myrec : Descendant_Type;

         procedure check_single_dep (innerpos : Pkgtypes.NV_Pairs.Cursor)
         is
            dep_nsv : Text renames Pkgtypes.NV_Pairs.Key (innerpos);
         begin
            myrec.descendents := myrec.descendents + 1;
            cache_map.Element (dep_nsv).dependencies.Iterate (check_single_dep'Access);
         end check_single_dep;
      begin
         myrec.nsv := Pkgtypes.NV_Pairs.Key (Position);
         myrec.descendents := 1;
         cache_map.Element (myrec.nsv).dependencies.Iterate (check_single_dep'Access);
         priority.Append (myrec);
      end calc;
   begin
      priority.Clear;
      depend_set.Iterate (calc'Access);
      Desc_sorter.Sort (priority);
   end calc_dependency_descendants;


   -----------------
   --  desc_desc  --
   -----------------
   function desc_desc (left, right : Descendant_Type) return Boolean is
   begin
      return left.descendents > right.descendents;
   end desc_desc;


   ------------------------------
   --  load_installation_data  --
   ------------------------------
   procedure load_installation_data
     (localdb     : Database.RDB_Connection;
      cache_map   : Pkgtypes.Package_Map.Map;
      install_map : in out  Pkgtypes.Package_Map.Map)
   is
      procedure get_local_package_data (Position : Pkgtypes.Package_Map.Cursor)
      is
         local_pkg : Pkgtypes.Package_Set.Vector;
         nsv       : constant String := USS (Pkgtypes.Package_Map.Key (Position));
      begin
         if INST.top_level_addition_list
           (db             => localdb,
            packages       => local_pkg,
            pattern        => nsv,
            override_exact => True)
         then
            if not local_pkg.Is_Empty then
               declare
                  new_rec : Pkgtypes.A_Package := local_pkg.Element (0);
               begin
                  install_map.Insert (SUS (nsv), new_rec);
               end;
            end if;
         end if;
      end get_local_package_data;
   begin
      cache_map.Iterate (get_local_package_data'Access);
   end load_installation_data;


   ---------------------------
   --  finalize_work_queue  --
   ---------------------------
   procedure finalize_work_queue
     (localdb       : Database.RDB_Connection;
      install_map   : Pkgtypes.Package_Map.Map;
      cache_map     : Pkgtypes.Package_Map.Map;
      priority      : Descendant_Set.Vector;
      opt_automatic : Boolean;
      opt_manual    : Boolean;
      opt_exactly   : Boolean;
      opt_force     : Boolean;
      opt_drop_deps : Boolean;
      queue         : in out Install_Order_Set.Vector)
   is
      prov_check   : Install_Order_Set.Vector;
      already_seen : Pkgtypes.NV_Pairs.Map;
      yes          : constant Text := SUS ("yes");

      function libraries_changed (ins_libs : Pkgtypes.Text_List.Vector;
                                  cat_libs : Pkgtypes.Text_List.Vector) return Boolean
      is
         --  Only iterate over installed libraries
         change_detected : Boolean := False;

         procedure check (Position : Pkgtypes.Text_List.Cursor)
         is
            installed_library : Text renames Pkgtypes.Text_List.Element (Position);
         begin
            if not cat_libs.Contains (installed_library) then
               change_detected := True;
            end if;
         end check;
      begin
         ins_libs.Iterate (check'Access);

         return change_detected;
      end libraries_changed;

      procedure drill_down (parent_level : Natural; deps : Descendant_Set.Vector)
      is
         this_level : constant Natural := parent_level + 1;

         procedure scan_dependency (Position : Descendant_Set.Cursor)
         is
            myrec : Install_Order_Type;
         begin
            myrec.nsv := Descendant_Set.Element (Position).nsv;
            myrec.level := this_level;
            if already_seen.Contains (myrec.nsv) then
               return;
            end if;

            already_seen.Insert (myrec.nsv, yes);
            if not install_map.Contains (myrec.nsv) then
               myrec.action    := new_install;
               myrec.automatic := True;
               if opt_exactly and then opt_manual then
                  myrec.automatic := False;
               end if;
               myrec.prov_lib_change := False;
            else
               myrec.automatic := install_map.Element (myrec.nsv).automatic;
               declare
                  cat_version : constant String := USS (cache_map.Element (myrec.nsv).version);
                  ins_version : constant String := USS (install_map.Element (myrec.nsv).version);
               begin
                  case Version.pkg_version_cmp (ins_version, cat_version) is

                     when 0 =>  --  same version

                        if not opt_force then
                           return;
                        end if;
                        myrec.action := reinstall;
                        myrec.prov_lib_change := False;  --  In theory, no prov change with reinstall

                     when 1 =>   --  downgrade

                        if not opt_force then
                           return;
                        end if;
                        myrec.action := upgrade;
                        myrec.prov_lib_change := libraries_changed
                          (install_map.Element (myrec.nsv).libs_provided,
                           cache_map.Element (myrec.nsv).libs_provided);

                     when -1 =>   --  upgrade

                        myrec.action := upgrade;
                        myrec.prov_lib_change := libraries_changed
                          (install_map.Element (myrec.nsv).libs_provided,
                           cache_map.Element (myrec.nsv).libs_provided);

                  end case;
               end;
            end if;
            queue.Append (myrec);
            if not opt_drop_deps then
               declare
                  subdep_priority : Descendant_Set.Vector;
               begin
                  calc_dependency_descendants
                    (depend_set => cache_map.Element (myrec.nsv).dependencies,
                     cache_map  => cache_map,
                     priority   => subdep_priority);
                  drill_down (this_level, subdep_priority);
               end;
            end if;
         end scan_dependency;
      begin
         deps.Iterate (scan_dependency'Access);
      end drill_down;

      procedure scan_top (priority_pos : Descendant_Set.Cursor)
      is
         myrec : Install_Order_Type;
      begin
         myrec.nsv := Descendant_Set.Element (priority_pos).nsv;
         myrec.level := 0;
         if already_seen.Contains (myrec.nsv) then
            return;
         end if;
         already_seen.Insert (myrec.nsv, yes);
         if not install_map.Contains (myrec.nsv) then
            myrec.action    := new_install;
            myrec.automatic := False;
            if opt_exactly and then opt_automatic then
               myrec.automatic := True;
            end if;
            myrec.prov_lib_change := False;
         else
            myrec.automatic := install_map.Element (myrec.nsv).automatic;
            declare
               cat_version : constant String := USS (cache_map.Element (myrec.nsv).version);
               ins_version : constant String := USS (install_map.Element (myrec.nsv).version);
            begin
               case Version.pkg_version_cmp (ins_version, cat_version) is

                  when 0 =>  --  same version

                     if not opt_force then
                        return;
                     end if;
                     myrec.action := reinstall;
                     myrec.prov_lib_change := False;  --  In theory, no prov change with reinstall

                  when 1 =>   --  downgrade

                     if not opt_force then
                        return;
                     end if;
                     myrec.action := upgrade;
                     myrec.prov_lib_change := libraries_changed
                       (install_map.Element (myrec.nsv).libs_provided,
                        cache_map.Element (myrec.nsv).libs_provided);

                  when -1 =>   --  upgrade

                     myrec.action := upgrade;
                     myrec.prov_lib_change := libraries_changed
                       (install_map.Element (myrec.nsv).libs_provided,
                        cache_map.Element (myrec.nsv).libs_provided);

               end case;
            end;
         end if;

         queue.Append (myrec);
         if not opt_drop_deps then
            declare
               dep_priority : Descendant_Set.Vector;
            begin
               calc_dependency_descendants
                 (depend_set => cache_map.Element (myrec.nsv).dependencies,
                  cache_map  => cache_map,
                  priority   => dep_priority);
                drill_down (myrec.level, dep_priority);
            end;
         end if;
      end scan_top;

      procedure filter_provides (Position : Install_Order_Set.Cursor)
      is
        myrec : Install_Order_Type renames Install_Order_Set.Element (Position);
      begin
         if myrec.prov_lib_change then
            prov_check.Append (myrec);
         end if;
      end filter_provides;

      procedure append_queue_from_lib_reinstalls (Position : Pkgtypes.Text_List.Cursor)
      is
         nsv : Text renames Pkgtypes.Text_List.Element (Position);
         myrec : Install_Order_Type;
      begin
         if not already_seen.Contains (nsv) then
            myrec.nsv    := nsv;
            myrec.level  := 0;
            myrec.action := reinstall;
            myrec.automatic := install_map.Element (nsv).automatic;
            myrec.prov_lib_change := False;
            queue.Append (myrec);
            already_seen.Insert (nsv, yes);
         end if;
      end append_queue_from_lib_reinstalls;

      procedure add_reinstallations (Position : Install_Order_Set.Cursor)
      is
         myrec : Install_Order_Type renames Install_Order_Set.Element (Position);

         procedure get_affected (libpos : Pkgtypes.Text_List.Cursor)
         is
            shared_library : Text renames Pkgtypes.Text_List.Element (libpos);
         begin
            if not cache_map.Element (myrec.nsv).libs_provided.Contains (shared_library) then
               declare
                  alist : Pkgtypes.Text_List.Vector;
               begin
                  INST.gather_packages_affected_by_libchange (localdb, USS (shared_library), alist);
                  alist.Iterate (append_queue_from_lib_reinstalls'Access);
               end;
            end if;
         end get_affected;
      begin
         install_map.Element (myrec.nsv).libs_provided.Iterate (get_affected'Access);
      end;
   begin
      already_seen.Clear;
      priority.Iterate (scan_top'Access);
      queue.iterate (filter_provides'Access);
      prov_check.Iterate (add_reinstallations'Access);

   end finalize_work_queue;


   ---------------------
   --  conflict_free  --
   ---------------------
   function conflict_free
     (queue           : Install_Order_Set.Vector;
      cache_map       : Pkgtypes.Package_Map.Map;
      file_collection : in out Pkgtypes.NV_Pairs.Map) return Boolean
   is
      conflict_found : Boolean := False;

      procedure check_package (Position : Install_Order_Set.Cursor)
      is
         operation  : Archive.Unpack.Darc;
         myrec : Install_Order_Type renames Install_Order_Set.Element (Position);
         rvn_path : constant String := RCU.config_setting (RCU.CFG.cachedir) & "/" &
           Pkgtypes.nsvv_identifier (cache_map (myrec.nsv)) & extension;
      begin
         case myrec.action is
            when reinstall => null;
            when new_install | upgrade =>
               declare
                  num_files : Natural;
                  shipped_files : Archive.Unpack.file_records.Vector;
               begin
                  operation.open_rvn_archive (rvn_path, Archive.silent);
                  if not operation.extract_manifest (shipped_files, "/") then
                     conflict_found := True;
                     Event.emit_error ("Unxpected error acquiring file list from " & rvn_path);
                     num_files := 0;
                  else
                     num_files := Natural (shipped_files.Length);
                  end if;
                  operation.close_rvn_archive;

                  if num_files > 0 then
                     for findex in 0 .. num_files - 1 loop
                        declare
                           fpath : Text := shipped_files.Element (findex).path;
                        begin
                           if file_collection.Contains (fpath) then
                              if not conflict_found then
                                 Event.emit_error ("");
                              end if;
                              conflict_found := True;
                              Event.emit_error
                                ("Conflict found: " & USS (myrec.nsv) &
                                   " package installs files in the same location as " &
                                   USS (file_collection.Element (fpath)));
                              exit;
                           end if;
                        end;
                     end loop;
                     for findex in 0 .. num_files - 1 loop
                        declare
                           fpath : Text := shipped_files.Element (findex).path;
                        begin
                           if not file_collection.Contains (fpath) then
                              file_collection.Insert (fpath, myrec.nsv);
                           end if;
                        end;
                     end loop;
                  end if;
               end;
         end case;
      end check_package;
   begin
      queue.Iterate (check_package'Access);
      return not conflict_found;
   end conflict_free;


   -------------------------------
   --  print_next_installation  --
   -------------------------------
   procedure print_next_installation (nextpkg  : Install_Order_Type;
                                      version  : String;
                                      counter  : Natural;
                                      total    : Natural)
   is
      function progress return String is
      begin
         if total = 0 then
            if counter < 10_000 then
               return pad_left (int2str (counter), 4) & ". ";
            end if;
            return pad_left (int2str (counter), 5) & ' ';  --  truncates from front at 100,000+
         end if;
          case total is
            when 0 .. 9 =>
               return "[" & int2str (counter) & "/" & int2str (total) & "] ";
            when 10 .. 99 =>
               return "[" & zeropad (counter, 2) & "/" & int2str (total) & "] ";
            when 100 .. 999 =>
               return "[" & zeropad (counter, 3) & "/" & int2str (total) & "] ";
            when 1000 .. 9999 =>
               return "[" & zeropad (counter, 4) & "/" & int2str (total) & "] ";
            when others =>
               return "[" & zeropad (counter, 5) & "/" & int2str (total) & "] ";
         end case;
      end progress;

      function star return String is
      begin
         case nextpkg.action is
            when reinstall   => return " [*]";
            when upgrade     => return " [U]";
            when new_install => return "";
         end case;
      end star;
   begin
      if nextpkg.level = 0 then
         Event.emit_message (progress & USS (nextpkg.nsv) & "-" & version & star);
         return;
      end if;

      declare
         vlast : Natural := 3 * nextpkg.level;
         verts : string (1 .. vlast) := (others => ' ');
         index : Natural := 1;
      begin
         for x in 1 .. nextpkg.level loop
            verts (index) := '|';
            index := index + 1;
            verts (index) := '`';
            index := index + 2;
         end loop;
         Event.emit_message (progress & verts & USS (nextpkg.nsv) & '-' & version & star);
      end;
   end print_next_installation;


   -------------------------------------
   --  granted_permission_to_proceed  --
   -------------------------------------
   function granted_permission_to_proceed (quiet : Boolean) return Boolean
   is
      cont : Character;
   begin
      if quiet or else RCU.config_setting (RCU.CFG.assume_yes) then
         return True;
      end if;

      Event.emit_message (LAT.LF & "Proceed with installing packages? [y/n]: ");
      Ada.Text_IO.Get_Immediate (cont);
      case cont is
         when 'Y' | 'y' => return True;
         when others => return False;
      end case;
   end granted_permission_to_proceed;


   procedure show_proposed_queue
     (queue        : Install_Order_Set.Vector;
      cache_map    : Pkgtypes.Package_Map.Map;
      install_map  : Pkgtypes.Package_Map.Map;
      behave_quiet : Boolean)
   is
      counter : Natural := 0;
      total_flatsize : Pkgtypes.Package_Size := 0;

      procedure increment (flatsize : Pkgtypes.Package_Size)
      is
         use type Pkgtypes.Package_Size;
      begin
         total_flatsize := total_flatsize + flatsize;
      end increment;

      procedure decrement (flatsize : Pkgtypes.Package_Size)
      is
         use type Pkgtypes.Package_Size;
      begin
         total_flatsize := total_flatsize - flatsize;
      end decrement;

      procedure display (Position : Install_Order_Set.Cursor)
      is
         myrec : Install_Order_Type renames Install_Order_Set.Element (Position);
         version : constant String := USS (cache_map.Element (myrec.nsv).version);
         already_installed : constant Boolean := install_map.Contains (myrec.nsv);
      begin
         counter := counter + 1;
         print_next_installation (myrec, version, counter, 0);
         case myrec.action is
            when new_install =>
               increment (cache_map.Element (myrec.nsv).flatsize);
            when reinstall | upgrade =>
               increment (cache_map.Element (myrec.nsv).flatsize);
               if already_installed then
                  decrement (install_map.Element (myrec.nsv).flatsize);
               end if;
         end case;
      end display;
   begin
      if behave_quiet then
         return;
      end if;
      Event.emit_message ("The following packages will be installed:" & LAT.LF);
      queue.Iterate (display'Access);
      Event.emit_message (LAT.LF & "Disk space required to install these packages: " &
                           Metadata.human_readable_size (int64 (total_flatsize)));
   end show_proposed_queue;


end Raven.Install;
