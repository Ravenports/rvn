--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Event;
with Raven.Context;
with Raven.Metadata;
with Raven.Deinstall;
with Raven.Miscellaneous;
with Raven.Database.Add;
with Raven.Database.Pkgs;
with Raven.Database.Lock;
with Raven.Database.Operations;
with Raven.Strings;
with Archive.Unpack;
with Archive.Unix;
with ThickUCL;

use Raven.Strings;


package body Raven.Install is

   package TIO  renames Ada.Text_IO;
   package MISC renames Raven.Miscellaneous;
   package INST renames Raven.Database.Add;
   package PKGS renames Raven.Database.Pkgs;
   package LOK  renames Raven.Database.Lock;
   package OPS  renames Raven.Database.Operations;

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
                                                      upgrading       => True);
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
                                                      upgrading       => False);
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
      upgrading         : Boolean) return Boolean
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
      --  Placeholder, needs to update graphically with indents and lines.
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

      operation.open_rvn_archive (archive_path, level, pipe_fd);
      good_extraction := operation.extract_archive
        (top_directory => root_directory,
         set_owners    => rootuser,
         set_perms     => rootuser,
         set_modtime   => False,
         skip_scripts  => inhibit_scripts,
         upgrading     => upgrading);
      operation.close_rvn_archive;

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
                                     patterns         : Pkgtypes.Text_List.Vector)
                                     return Boolean
   is
      --  case-sensitive accessed via RCU settings
      --  assume-yes accessed via RCU settings

      rdb         : Database.RDB_Connection;
      active_lock : LOK.lock_type := LOK.lock_advisory;
      succeeded   : Boolean;
      released    : Boolean;
      catalog_map : Pkgtypes.Package_Map.Map;

      function release_active_lock return Boolean is
      begin
         if not LOK.release_lock (rdb, active_lock) then
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

      if not LOK.obtain_lock (rdb, active_lock) then
         case active_lock is
            when LOK.lock_advisory  => Event.emit_error (LOK.no_adv_lock);
            when LOK.lock_exclusive => Event.emit_error (LOK.no_exc_lock);
            when LOK.lock_readonly  => Event.emit_error (LOK.no_read_lock);
         end case;
         OPS.rdb_close (rdb);
         return False;
      end if;

      succeeded := assemble_work_queue (rdb, opt_exact_match, patterns, catalog_map);
      if succeeded then
         declare
            priority  : Descendant_Set.Vector;
            cache_map : Pkgtypes.Package_Map.Map;
            --  procedure print (Position : Descendant_Set.Cursor) is
            --  begin
            --     Event.emit_message (USS (Descendant_Set.Element (Position).nsv) & "  priority="
            --                           & int2str(Descendant_Set.Element (Position).descendents));
            --  end print;
         begin
            calculate_descendants (rdb, catalog_map, cache_map, priority);
            --  priority.Iterate (print'Access);
         end;
      end if;

      released := release_active_lock;
      OPS.rdb_close (rdb);
      return succeeded and then released;

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


   -----------------
   --  desc_desc  --
   -----------------
   function desc_desc (left, right : Descendant_Type) return Boolean is
   begin
      return left.descendents > right.descendents;
   end desc_desc;

end Raven.Install;
