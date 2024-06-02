--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Directories;
with Lua;
with Bourne;
with Blake_3;
with Raven.Event;
with Raven.Strings;
with Archive.Misc;
with Archive.Unix;
with Archive.Dirent.Scan;

use Raven.Strings;

package body Raven.Deinstall is

   package SCN renames Archive.Dirent.Scan;


   -----------------------------------
   --  deinstall_extracted_package  --
   -----------------------------------
   procedure deinstall_extracted_package (installed_package   : Pkgtypes.A_Package;
                                          verify_digest_first : Boolean;
                                          quiet               : Boolean;
                                          post_report         : TIO.File_Type)
   is
      --  Do not use for upgrades

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
      --  Step 9.  Delete tmo output files of lua and shell scripts
      --  Call needs to deregister from database (assumed to be successful?)

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
                        if quiet then
                           Event.emit_message (nsv & ": deleting modified file " & path);
                        end if;
                     end if;
                  end;
               when others => null;
            end case;
         end if;
         if not Archive.Unix.unlink_file (path) then
            Event.emit_message (nsv & ": failed to delete " & path);
         end if;
      end eradicate_file;
   begin
      run_shell_scripts (ARW.pre_deinstall, installed_package, upgrading, tmp_message_shell);
      run_lua_scripts (ARW.pre_deinstall_lua, installed_package, upgrading, tmp_message_lua);

      installed_package.files.Iterate (eradicate_file'Access);
      prune_empty_directories (installed_package);

      run_shell_scripts (ARW.post_deinstall, installed_package, upgrading, tmp_message_shell);
      run_lua_scripts (ARW.post_deinstall_lua, installed_package, upgrading, tmp_message_lua);

      --  redirect
      TIO.Set_Output (post_report);
      Bourne.show_post_run_messages (tmp_message_shell, z_namebase, z_subpackage, z_variant);
      Lua.show_post_run_messages (tmp_message_lua, z_namebase, z_subpackage, z_variant);
      TIO.Set_Output (TIO.Standard_Output);

      --  clean up
      if not Archive.Unix.unlink_file (tmp_message_shell) then
         null;
      end if;
      if not Archive.Unix.unlink_file (tmp_message_lua) then
         null;
      end if;
   end deinstall_extracted_package;


   -------------------------
   --  run_shell_scripts  --
   -------------------------
   procedure run_shell_scripts
     (phase       : ARW.package_phase;
      the_package : Pkgtypes.A_Package;
      upgrading   : Boolean;
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
               root_dir    => "/",
               upgrading   => upgrading,
               interpreter => interpreter,
               script      => USS (the_package.scripts (phase)(script_index).code),
               arguments   => USS (the_package.scripts (phase)(script_index).args),
               msg_outfile => msg_outfile,
               success     => success);
            if not success then
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
               root_dir    => "/",
               upgrading   => upgrading,
               script      => USS (the_package.scripts (phase)(script_index).code),
               arg_chain   => USS (the_package.scripts (phase)(script_index).args),
               msg_outfile => msg_outfile,
               success     => success);
            if not success then
               Event.emit_notice ("Lua script" & script_index'Img & " failed");
            end if;
         end;
      end loop;
   end run_lua_scripts;


   -------------------------------
   --  prune_empty_directories  --
   -------------------------------
   procedure prune_empty_directories (the_package : Pkgtypes.A_Package)
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

      procedure add_dirs (Position : Pkgtypes.Text_List.Cursor)
      is
         owned_dir : constant String := USS (Pkgtypes.Text_List.Element (Position));
         slash     : constant Natural := count_char (owned_dir, '/');
      begin
         dirmap.Insert (Key      => Pkgtypes.Text_List.Element (Position),
                        New_Item => SUS (int2str (slash)));
      end add_dirs;

      procedure check_file (Position : Pkgtypes.File_List.Cursor)
      is
         full_path  : Text renames Pkgtypes.File_List.Element (Position).path;
         parent_dir : constant Text := head (full_path, SUS ("/"));
      begin
         if not on_blacklist (parent_dir) and then
           not dirmap.Contains (parent_dir)
         then
            declare
               slash : constant Natural := count_char (USS (parent_dir), '/');
            begin
               dirmap.Insert (parent_dir, SUS (int2str (slash)));
            end;
         end if;
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


end Raven.Deinstall;
