--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Directories;
with Ada.Characters.Latin_1;
with Lua;
with Bourne;
with Blake_3;
with Raven.Event;
with Raven.Strings;
with Raven.Triggers;
with Raven.Cmd.Unset;
with Raven.Database.Remove;
with Raven.Miscellaneous;
with Archive.Misc;
with Archive.Unix;
with Archive.Dirent.Scan;

use Raven.Strings;

package body Raven.Deinstall is

   package SCN renames Archive.Dirent.Scan;
   package LAT renames Ada.Characters.Latin_1;
   package DEL renames Raven.Database.Remove;
   package RCU renames Raven.Cmd.Unset;


   -----------------------------------
   --  deinstall_extracted_package  --
   -----------------------------------
   procedure deinstall_extracted_package (installed_package   : Pkgtypes.A_Package;
                                          verify_digest_first : Boolean;
                                          quiet               : Boolean;
                                          inhibit_scripts     : Boolean;
                                          rootdir             : String;
                                          post_report         : TIO.File_Type)
   is
      --  Step 1.  run pre-deinstall shell scripts (create tmp output file first)
      --  Step 2.  run pre-deinstall lua scripts (create tmp output file first)
      --  Step 3.  iterate file container and delete each one.
      --           Note: there is no check if the file is registered to multiple packages.
      --           Even if it were, we don't know which version won
      --           Optionally determine if the file changes since installation by verifying
      --           checksum.  If it fails, write to message handle
      --  Step 4.  Iterate directories container.
      --           If the directory exists and is empty, remove it
      --  Step 5.  run post-deinstall shell scripts
      --  Step 6.  run post-deinstall lua scripts
      --  Step 7.  echo shell script output (if any) (caller might have redirected to file)
      --  Step 8.  echo lua script output (if any) (caller might have redirected to file)
      --  Step 9.  Delete two output files of lua and shell scripts
      --  Caller needs to deregister from database (assumed to be successful?)

      upgrading         : constant Boolean := False;
      tmp_message_shell : constant String := Bourne.unique_msgfile_path;
      tmp_message_lua   : constant String := Lua.unique_msgfile_path;
      z_namebase        : constant String := USS (installed_package.namebase);
      z_subpackage      : constant String := USS (installed_package.subpackage);
      z_variant         : constant String := USS (installed_package.variant);
      nsv               : constant String := z_namebase & '-' & z_subpackage & '-' & z_variant;

      procedure eradicate_file (Position : Pkgtypes.File_List.Cursor)
      is
         path : constant String := USS (Pkgtypes.File_List.Element (Position).path);
         features : Archive.Unix.File_Characteristics;
      begin
         features := Archive.Unix.get_charactistics (path);
         case features.ftype is
            when Archive.unsupported =>
               if not quiet then
                  Event.emit_error (nsv & ": absent file slated for deletion: " & path);
               end if;
               return;
            when Archive.directory =>
               Event.emit_error (nsv & ": directory expected to be a file: " & path);
            when Archive.regular | Archive.symlink | Archive.hardlink | Archive.fifo =>
               null;
         end case;
         if verify_digest_first then
            --  don't verify symlinks or FIFO (directories, unsupported impossible)
            case features.ftype is
               when Archive.regular | Archive.hardlink =>
                  declare
                     current_b3sum : Blake_3.blake3_hash_hex;
                  begin
                     current_b3sum := Blake_3.hex (Blake_3.file_digest (path));
                     if current_b3sum /= Pkgtypes.File_List.Element (Position).digest then
                        if not quiet then
                           Event.emit_message (nsv & ": deleting modified file " & path);
                        end if;
                     end if;
                  end;
               when others => null;
            end case;
         end if;
         if not Archive.Unix.unlink_file (path) then
            if not quiet then
               Event.emit_message (nsv & ": failed to delete " & path);
            end if;
         end if;
      end eradicate_file;
   begin
      Event.emit_remove_begin (installed_package);
      Event.emit_debug (moderate, "Deinstalling " & Pkgtypes.nsv_identifier (installed_package));
      if not inhibit_scripts then
         Event.emit_debug (moderate, "Running pre-deinstall Bourne shell scripts");
         run_shell_scripts (ARW.pre_deinstall, installed_package, upgrading, rootdir,
                            tmp_message_shell);
         Event.emit_debug (moderate, "Running pre-deinstall Lua scripts");
         run_lua_scripts (ARW.pre_deinstall_lua, installed_package, upgrading, rootdir,
                          tmp_message_lua);
      end if;

      Event.emit_debug (moderate, "Removing files");
      installed_package.files.Iterate (eradicate_file'Access);
      prune_empty_directories (installed_package, rootdir);

      if not inhibit_scripts then
         Event.emit_debug (moderate, "Running post-deinstall Bourne shell scripts");
         run_shell_scripts (ARW.post_deinstall, installed_package, upgrading, rootdir,
                            tmp_message_shell);
         Event.emit_debug (moderate, "Running post-deinstall Lua scripts");
         run_lua_scripts (ARW.post_deinstall_lua, installed_package, upgrading, rootdir,
                          tmp_message_lua);

         Bourne.show_post_run_messages (tmp_message_shell, z_namebase, z_subpackage, z_variant,
                                        post_report);
         Lua.show_post_run_messages (tmp_message_lua, z_namebase, z_subpackage, z_variant,
                                     post_report);
      end if;

      show_deinstallation_messages (installed_package, post_report);

      --  clean up
      if not Archive.Unix.unlink_file (tmp_message_shell) then
         null;
      end if;
      if not Archive.Unix.unlink_file (tmp_message_lua) then
         null;
      end if;
      Event.emit_debug (moderate, "Deinstall complete");
      Event.emit_remove_end (installed_package);
   end deinstall_extracted_package;


   -------------------------
   --  run_shell_scripts  --
   -------------------------
   procedure run_shell_scripts
     (phase       : ARW.package_phase;
      the_package : Pkgtypes.A_Package;
      upgrading   : Boolean;
      rootdir     : String;
      msg_outfile : String)
   is
      num_scripts : Natural;
      interpreter : constant String := Archive.Misc.get_interpreter;
   begin
      case phase is
         when ARW.pre_deinstall => null;
         when ARW.post_deinstall => null;
         when others =>
            Event.emit_message ("run_shell_scripts: developer error");
            return;
      end case;
      if the_package.scripts (phase).Is_Empty then
         return;
      end if;
      num_scripts := Natural (the_package.scripts (phase).Length);
      for script_index in 0 .. num_scripts - 1 loop
         declare
            success : Boolean;
         begin
            Bourne.run_shell_script
              (namebase    => USS (the_package.namebase),
               subpackage  => USS (the_package.subpackage),
               variant     => USS (the_package.variant),
               prefix      => USS (the_package.prefix),
               root_dir    => rootdir,
               upgrading   => upgrading,
               interpreter => interpreter,
               script      => USS (the_package.scripts (phase)(script_index).code),
               arguments   => USS (the_package.scripts (phase)(script_index).args),
               msg_outfile => msg_outfile,
               success     => success);
            if not success then
               case phase is
                  when ARW.pre_deinstall  => Event.emit_premessage ("PRE-DEINSTALL: ");
                  when ARW.post_deinstall => Event.emit_premessage ("POST-DEINSTALL: ");
                  when others => null;
               end case;
               Event.emit_notice ("Bourne script" & script_index'Img & " failed");
            end if;
         end;
      end loop;
   end run_shell_scripts;


   -----------------------
   --  run_lua_scripts  --
   -----------------------
   procedure run_lua_scripts
     (phase       : ARW.package_phase;
      the_package : Pkgtypes.A_Package;
      upgrading   : Boolean;
      rootdir     : String;
      msg_outfile : String)
   is
      num_scripts : Natural;
   begin
      case phase is
         when ARW.pre_deinstall_lua => null;
         when ARW.post_deinstall_lua => null;
         when others =>
            Event.emit_message ("run_lua_scripts: developer error");
            return;
      end case;
      if the_package.scripts (phase).Is_Empty then
         return;
      end if;
      num_scripts := Natural (the_package.scripts (phase).Length);
      for script_index in 0 .. num_scripts - 1 loop
         declare
            success : Boolean;
         begin
            Lua.run_lua_script
              (namebase    => USS (the_package.namebase),
               subpackage  => USS (the_package.subpackage),
               variant     => USS (the_package.variant),
               prefix      => USS (the_package.prefix),
               root_dir    => rootdir,
               upgrading   => upgrading,
               script      => USS (the_package.scripts (phase)(script_index).code),
               arg_chain   => USS (the_package.scripts (phase)(script_index).args),
               msg_outfile => msg_outfile,
               success     => success);
            if not success then
               case phase is
                  when ARW.pre_deinstall  => Event.emit_premessage ("PRE-DEINSTALL-LUA: ");
                  when ARW.post_deinstall => Event.emit_premessage ("POST-DEINSTALL-LUA: ");
                  when others => null;
               end case;
               Event.emit_notice ("Script" & script_index'Img & " failed");
            end if;
         end;
      end loop;
   end run_lua_scripts;


   -------------------------------
   --  prune_empty_directories  --
   -------------------------------
   procedure prune_empty_directories (the_package : Pkgtypes.A_Package; extract_loc : String)
   is
      dirmap : Pkgtypes.NV_Pairs.Map;
      queue  : Pkgtypes.Text_List.Vector;
      dircount  : Natural := 0;
      dircountx : Natural;

      function on_blacklist (parent_dir : Text) return Boolean
      is
         --  e.g. /raven/bin
         --  e.g. /var/run
         dirstr : constant String := USS (parent_dir);
         topdir  : constant String := specific_field (dirstr, 2, "/");
      begin
         if topdir = "usr" or else
           topdir = "etc" or else
           topdir = "bin" or else
           topdir = "sbin" or else
           topdir = "lib" or else
           topdir = "libexec"
         then
            --  gross validity check
            return True;
         end if;

         if dirstr = "/var/db" or else
           dirstr = "/var/cache" or else
           dirstr = "/var/run" or else
           dirstr = "/var/log"
         then
            return True;
         end if;
         return False;
      end;

      procedure add_head (mydir : String)
      is
         numslash : constant Natural := count_char (mydir, '/');
         mydir_txt : constant Text := SUS (mydir);
      begin
         Event.emit_debug (moderate, "add_head: " & mydir);
         if not dirmap.Contains (mydir_txt) then
            if not on_blacklist (mydir_txt) then
               dirmap.Insert (mydir_txt, SUS (int2str (numslash)));
            end if;
         end if;
         if numslash > 1 then
            add_head (head (mydir, "/"));
         end if;
      end add_head;

      procedure add_dirs (Position : Pkgtypes.Text_List.Cursor)
      is
         owned_dir : constant String := extract_loc & "/" &
           USS (Pkgtypes.Text_List.Element (Position));
      begin
         add_head (owned_dir);
      end add_dirs;

      procedure check_file (Position : Pkgtypes.File_List.Cursor)
      is
         full_path  : Text renames Pkgtypes.File_List.Element (Position).path;
         parent_dir : constant String := head (USS (full_path), "/");
      begin
         add_head (parent_dir);
      end check_file;

      procedure set_dircount (Position : Pkgtypes.NV_Pairs.Cursor)
      is
         this_count : Natural;
      begin
         this_count := Natural'value (USS (Pkgtypes.NV_Pairs.Element (Position)));
         if this_count > dircount then
            dircount := this_count;
         end if;
      end set_dircount;

      procedure select_by_dircount (Position : Pkgtypes.NV_Pairs.Cursor)
      is
         key : Text renames Pkgtypes.NV_Pairs.Key (Position);
         this_count : Natural;
      begin
         this_count := Natural'Value (USS (dirmap.Element (key)));
         if this_count = dircountx then
            queue.Append (key);
         end if;
      end select_by_dircount;

      procedure remove_from_map (Position : Pkgtypes.Text_List.Cursor)
      is
         key : Text renames Pkgtypes.Text_List.Element (Position);
      begin
         dirmap.Delete (key);
      end;

      procedure drop_if_empty (Position : Pkgtypes.Text_List.Cursor)
      is
         dirpath  : constant String := USS (Pkgtypes.Text_List.Element (Position));
         contents : SCN.dscan_crate.Vector;
         features : Archive.Unix.File_Characteristics;
      begin
         features := Archive.Unix.get_charactistics (dirpath);
         case features.ftype is
            when Archive.directory =>
               SCN.scan_directory (dirpath, contents);
               if contents.Is_Empty then
                  Ada.Directories.Delete_Directory (dirpath);
               end if;
            when Archive.unsupported =>
               Event.emit_debug (high_level, "Expected directory does not exist: " & dirpath);
            when others =>
               Event.emit_debug (high_level, "Expected directory at " & dirpath &
                                   ", but is " & features.ftype'Img);
         end case;
      end drop_if_empty;

   begin
      Event.emit_debug (moderate, "number directories is " & the_package.directories.Length'Img);
      the_package.directories.Iterate (add_dirs'Access);
      the_package.files.Iterate (check_file'Access);
      dirmap.Iterate (set_dircount'Access);
      for x in reverse 1 .. dircount loop
         queue.clear;
         dircountx := x;
         dirmap.Iterate (select_by_dircount'Access);
         queue.Iterate (drop_if_empty'Access);
         queue.Iterate (remove_from_map'Access);
      end loop;
   end prune_empty_directories;


   -----------------------------
   --  determine_purge_order  --
   -----------------------------
   procedure determine_purge_order
     (purge_list  : Pkgtypes.Package_Set.Vector;
      purge_order : in out Purge_Order_Crate.Vector)
   is
      delim    : constant String (1 .. 1) := (others => LAT.HT);
      counter  : Natural := 0;

      procedure scan (Position : Pkgtypes.Package_Set.Cursor)
      is
         mypkg : Pkgtypes.A_Package renames Pkgtypes.Package_Set.Element (Position);
      begin
         declare
            nsvplus : constant String :=
              Pkgtypes.nsv_identifier (mypkg) & delim & int2str (counter);
         begin
            event.emit_debug (high_level, "purge list: " & nsvplus);
            purge_order.Prepend (counter);
            counter := counter + 1;
         end;
      end scan;

      procedure set_order (Position : Pkgtypes.Text_List.Cursor)
      is
         deux : constant String := part_2 (USS (Pkgtypes.Text_List.Element (Position)), delim);
      begin
         purge_order.Append (Natural'Value (deux));
      end set_order;
   begin
      purge_order.Clear;
      purge_list.Iterate (scan'Access);
   end determine_purge_order;


   ----------------------------
   --  format_removal_order  --
   ----------------------------
   function format_removal_order (counter : Natural) return String is
   begin
      if counter < 10_000 then
         return pad_left (int2str (counter), 4) & '.';
      end if;
      return pad_left (int2str (counter), 5);  --  truncates from front at 100,000+
   end format_removal_order;


   ---------------------------
   --  show_proposed_queue  --
   ---------------------------
   procedure show_proposed_queue
     (purge_list     : Pkgtypes.Package_Set.Vector;
      purge_order    : Purge_Order_Crate.Vector;
      behave_quiet   : Boolean;
      dryrun         : Boolean)
   is
      --  Display format
      --   6 chars: right-padded 5 spaces counter, plus + space
      --  73 chars: display nsv + version (right just).
      --            If nsv too long, replace last char with *
      --  --------------
      --  79 chars

      counter : Natural := 0;

      procedure display_line (Position : Purge_Order_Crate.Cursor)
      is
         plndx     : constant Natural := Purge_Order_Crate.Element (Position);
         myrec     : Pkgtypes.A_Package renames purge_list.Element (plndx);
         full_line : String (1 .. 79) := (others => ' ');
         max_nsv   : Natural;
      begin
         counter := counter + 1;
         full_line (1 .. 5) := format_removal_order (counter);
         declare
            nsv : constant String := Pkgtypes.nsv_identifier (myrec);
            ver : constant String := "version: " & USS (myrec.version);
            vstart : constant Natural := full_line'Last - ver'Length + 1;
         begin
            full_line (vstart .. full_line'Last) := ver;
            max_nsv := 73 - (ver'Length + 1);
            if nsv'Length > max_nsv then
               full_line (7 .. 6 + max_nsv - 1) := nsv (nsv'First .. nsv'First + max_nsv - 2);
               full_line (6 + max_nsv) := '*';
            else
               full_line (7 .. 6 + nsv'Length) := nsv;
            end if;
         end;
         Event.emit_message (full_line);
      end display_line;

   begin
      if behave_quiet then
         return;
      end if;

      if dryrun then
         Event.emit_premessage ("Dry run: ");
      end if;
      Event.emit_message ("The following packages will be removed:" & LAT.LF);
      purge_order.Iterate (display_line'Access);
   end show_proposed_queue;


   -------------------------------------
   --  granted_permission_to_proceed  --
   -------------------------------------
   function granted_permission_to_proceed return Boolean
   is
      cont : Character;
   begin
      if RCU.config_setting (RCU.CFG.assume_yes) then
         return True;
      end if;

      Event.emit_message (LAT.LF & "Proceed with removing selected installed packages? [y/n]: ");
      Ada.Text_IO.Get_Immediate (cont);
      case cont is
         when 'Y' | 'y' => return True;
         when others => return False;
      end case;
   end granted_permission_to_proceed;


   --------------------------------
   --  remove_packages_in_order  --
   --------------------------------
   procedure remove_packages_in_order
     (rdb            : Database.RDB_Connection;
      purge_list     : Pkgtypes.Package_Set.Vector;
      purge_order    : Purge_Order_Crate.Vector;
      skip_verify    : Boolean;
      skip_scripts   : Boolean;
      quiet          : Boolean;
      rootdir        : String)
   is
      tmp_filename  : constant String := Miscellaneous.get_temporary_filename ("remove");
      total_pkgs    : constant Natural := Natural (purge_order.Length);
      deinstall_log : TIO.File_Type;
      pkg_counter   : Natural := 0;
      trigger_set   : Triggers.A_Trigger_Set;

      function progress return String is
      begin
         case total_pkgs is
            when 0 .. 9 =>
               return "[" & int2str (pkg_counter) & "/" & int2str (pkg_counter) & "] ";
            when 10 .. 99 =>
               return "[" & zeropad (pkg_counter, 2) & "/" & int2str (pkg_counter) & "] ";
            when 100 .. 999 =>
               return "[" & zeropad (pkg_counter, 3) & "/" & int2str (pkg_counter) & "] ";
            when 1000 .. 9999 =>
               return "[" & zeropad (pkg_counter, 4) & "/" & int2str (pkg_counter) & "] ";
            when others =>
               return "[" & zeropad (pkg_counter, 5) & "/" & int2str (pkg_counter) & "] ";
         end case;
      end progress;

      procedure print_removal_instruction (this_package : Pkgtypes.A_Package)
      is
         info     : constant String := "Removing " & Pkgtypes.nsvv_identifier (this_package);
         fragment : constant String := progress;
         fragsize : constant Natural := fragment'Length;
         max_size : constant Natural := 79 - 1 - fragsize;
      begin
         Event.emit_premessage (fragment);
         if info'Length > max_size then
            Event.emit_message (info (info'First .. info'First + max_size - 2) & '*');
         else
            Event.emit_message (info);
         end if;
      end print_removal_instruction;

      procedure remove_installed_package (Position : Purge_Order_Crate.Cursor)
      is
         purge_index : constant Natural := Purge_Order_Crate.Element (Position);
         mypackage   : Pkgtypes.A_Package renames purge_list.Element (purge_index);

         procedure activate_cleanup (Position : Pkgtypes.Trigger_List.Cursor)
         is
            this_trigger : Pkgtypes.A_Trigger renames Pkgtypes.Trigger_List.Element (Position);
         begin
            if not IsBlank (this_trigger.cleanup_script) then
               trigger_set.insert
                 (trigger_id      => this_trigger.trigger_id,
                  trigger_type    => Triggers.cleanup,
                  script_code     => USS (this_trigger.cleanup_script),
                  entity_path     => "",
                  origin_namebase => USS (mypackage.namebase),
                  origin_subpkg   => USS (mypackage.subpackage),
                  origin_variant  => USS (mypackage.variant),
                  origin_prefix   => USS (mypackage.prefix),
                  upgrading       => False);
            end if;
         end activate_cleanup;

      begin
         pkg_counter := pkg_counter + 1;
         if not quiet then
            print_removal_instruction (mypackage);
         end if;
         deinstall_extracted_package
           (installed_package   => mypackage,
            verify_digest_first => not skip_verify,
            quiet               => quiet,
            inhibit_scripts     => skip_scripts,
            rootdir             => rootdir,
            post_report         => deinstall_log);
         DEL.drop_package_with_cascade (rdb, mypackage.id);
         mypackage.triggers.Iterate (activate_cleanup'Access);
      end remove_installed_package;
   begin
      trigger_set.set_rootdir (rootdir);
      TIO.Create (deinstall_log, TIO.Out_File, tmp_filename);
      purge_order.Iterate (remove_installed_package'Access);
      TIO.Close (deinstall_log);

      if Pkgtypes.">" (Pkgtypes.get_file_size (tmp_filename), 5) then
         Event.emit_message ("");
         TIO.Open (deinstall_log, TIO.In_File, tmp_filename);
         while not  TIO.End_Of_File (deinstall_log) Loop
            Event.emit_message (TIO.Get_Line (deinstall_log));
         end loop;
         TIO.Close (deinstall_log);
      end if;

      if Archive.Unix.file_exists (tmp_filename) then
         if not Archive.Unix.unlink_file (tmp_filename) then
            Event.emit_debug (moderate, "Failed to unlink temporary file " & tmp_filename);
         end if;
      end if;
      trigger_set.execute;
   end remove_packages_in_order;


   ------------------------------------
   --  show_deinstallation_messages  --
   ------------------------------------
   procedure show_deinstallation_messages
     (the_package : Pkgtypes.A_Package;
      post_report : TIO.File_Type)
   is
      redirected : constant Boolean := TIO.Is_Open (post_report);
      msg : constant String := Pkgtypes.combined_messages (the_package, Pkgtypes.deinstall, "");
      divlength : constant Natural := 75;
      partone : constant String := Pkgtypes.nsv_identifier (the_package) &
        " deinstallation messages  ";
      divider : String (1 .. divlength) := (others => '-');
   begin
      if IsBlank (msg) then
         return;
      end if;
      if redirected then
         if partone'Length > divlength then
            divider := partone (partone'First .. partone'First + divlength - 1);
         else
            divider (divider'First .. divider'First + partone'Length - 1) := partone;
         end if;
         TIO.Put_Line (post_report, divider);
         TIO.Put_Line (post_report, msg);
      else
         Event.emit_message (msg);
      end if;
   end show_deinstallation_messages;

end Raven.Deinstall;
