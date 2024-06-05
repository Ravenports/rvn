--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Event;
with Raven.Context;
with Raven.Pkgtypes;
with Raven.Cmd.Unset;
with Raven.Miscellaneous;
with Raven.Database.Pkgs;
with Raven.Database.Lock;
with Raven.Database.Query;
with Raven.Database.Operations;
with Raven.Strings; use Raven.Strings;
with Archive.Unix;


package body Raven.Cmd.Install is

   package EV   renames Raven.Event;
   package RCU  renames Raven.Cmd.Unset;
   package MISC renames Raven.Miscellaneous;
   package PKGS renames Raven.Database.Pkgs;
   package LOK  renames Raven.Database.Lock;
   package QRY  renames Raven.Database.Query;
   package OPS  renames Raven.Database.Operations;

   -------------------------------
   --  execute_install_command  --
   -------------------------------
   function execute_install_command (comline : Cldata) return Boolean
   is
      install_success : Boolean;
      rdb : Database.RDB_Connection;
   begin
      if comline.common_options.case_sensitive then
         return currently_unsupported ("--case-sensitive");
      end if;
      if comline.common_options.exact_match then
         return currently_unsupported ("--exact-match");
      end if;
      if comline.common_options.no_repo_update then
         return currently_unsupported ("--no-repo-update");
      end if;
      if comline.common_options.assume_yes then
         return currently_unsupported ("--assume-yes");
      end if;
      if comline.cmd_install.manual then
         return currently_unsupported ("--manual");
      end if;
      if comline.cmd_install.automatic then
         return currently_unsupported ("--automatic");
      end if;
      if comline.cmd_install.fetch_only then
         return currently_unsupported ("--fetch-only");
      end if;
      if comline.cmd_install.force_install then
         return currently_unsupported ("--force");
      end if;
      if comline.cmd_install.drop_depends then
         return currently_unsupported ("--drop-depends");
      end if;

      if comline.cmd_install.local_file then
         if not comline.cmd_install.no_register then
            case OPS.rdb_open_localdb (rdb, Database.installed_packages) is
               when RESULT_OK => null;
               when others => return False;
            end case;

            if not LOK.obtain_lock (rdb, LOK.lock_exclusive) then
               Event.emit_error (LOK.no_exc_lock);
               OPS.rdb_close (rdb);
               return False;
            end if;

            install_success := install_single_local_package (rdb, comline);

            if not LOK.release_lock (rdb, LOK.lock_exclusive) then
               Event.emit_error (LOK.no_exc_lock);
               OPS.rdb_close (rdb);
               return False;
            end if;

            OPS.rdb_close (rdb);
         else
            install_success := install_single_local_package (rdb, comline);
         end if;

         return install_success;
      else
         Raven.Event.emit_error ("Installation from repository not yet supported");
         return False;
      end if;

   end execute_install_command;


   -----------------------------
   --  currently_unsupported  --
   -----------------------------
   function currently_unsupported (switch : String) return Boolean is
   begin
      EV.emit_error ("switch " & switch & " is currently unsupported.");
      return False;
   end currently_unsupported;


   -----------------------------------
   --  install_single_local_package  --
   ------------------------------------
   function install_single_local_package (rdb : in out Database.RDB_Connection;
                                          comline : Cldata) return Boolean
   is
      file_list    : EXT.file_records.Vector;
      metatree     : ThickUCL.UclTree;
      operation    : Archive.Unpack.Darc;
      result       : Boolean := True;
      skip_scripts : Boolean;

      function archive_path return String is
      begin
         return USS (comline.common_options.multiple_patterns.Element (0));
      end archive_path;

      function extract_location return String
      is
         rootdir : constant String := USS (comline.pre_command.install_rootdir);
      begin
         if rootdir = "" then
            return "/";
         end if;
         return rootdir;
      end extract_location;
   begin

      operation.open_rvn_archive (archive_path, Archive.silent, Archive.Unix.not_connected);
      if not operation.extract_manifest (file_list, extract_location) then
         EV.emit_error ("Failed to extract manifest of packaged files.");
      end if;
      operation.populate_metadata_tree (metatree);
      operation.close_rvn_archive;

      declare
         N : constant String := MET.reveal_namebase (metatree);
         S : constant String := MET.reveal_subpackage (metatree);
         V : constant String := MET.reveal_variant (metatree);
         P : constant String := N & "-" & S & "-" & V;
      begin
         if not comline.cmd_install.no_register then
            if not comline.cmd_install.force_install then
               case QRY.package_installed (rdb, N, S, V) is
                  when Pkgtypes.Package_Not_Installed => null;
                  when others =>
                     EV.emit_error ("The " & P & " package is already installed.");
                     return False;
               end case;
            end if;
         end if;

         if comline.cmd_install.only_register then
            return register_single_package
              (rdb, metatree, file_list, comline.cmd_install.automatic,
               comline.cmd_install.force_install);
         end if;

         if not comline.cmd_install.no_register then
            result := register_single_package
              (rdb, metatree, file_list, comline.cmd_install.automatic,
               comline.cmd_install.force_install);
         end if;

         if comline.cmd_install.inhibit_scripts then
            skip_scripts := True;
         else
            skip_scripts := not RCU.config_setting (RCU.CFG.run_scripts);
         end if;


         if result then
            Event.emit_install_begin (N, S, V, MET.reveal_version (metatree));

            result := install_files_from_archive
              (archive_path    => archive_path,
               root_directory  => extract_location,
               inhibit_scripts => skip_scripts,
               be_silent       => comline.common_options.quiet,
               dry_run_only    => comline.common_options.dry_run,
               upgrading       => False);

            declare
               function end_message return String is
               begin
                  if result then
                     return "successful";
                  end if;
                  return "installation failed";
               end end_message;
            begin
               Event.emit_install_end (N, S, V, MET.reveal_version (metatree), end_message);
            end;
         end if;
      end;

      return result;
   end install_single_local_package;


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
         EV.emit_message ("dry-run: " & action & " " & basename & " package");
         return True;
      else
         if not be_silent then
            EV.emit_notice (action & " " & basename & " package");
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
   --  register_single_package  --
   -------------------------------
   function register_single_package
     (rdb            : in out Database.RDB_Connection;
      metatree       : ThickUCL.UclTree;
      file_list      : EXT.file_records.Vector;
      mark_automatic : Boolean;
      force_install  : Boolean) return Boolean
   is
      my_package : Pkgtypes.A_Package;
   begin
      Metadata.convert_to_package (metatree, file_list, my_package, mark_automatic);
      return PKGS.rdb_register_package (rdb, my_package, force_install);
   end register_single_package;

end Raven.Cmd.Install;
