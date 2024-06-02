--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Text_IO;
with Ada.Characters.Latin_1;
with Raven.Event;
with Raven.Cmd.Unset;
with Raven.Deinstall;
with Raven.Database.Remove;
with Raven.Database.Operations;
with Raven.Miscellaneous;
with Raven.Strings;
with Archive.Unix;

use Raven.Strings;

package body Raven.Cmd.Remove is

   package TIO renames Ada.Text_IO;
   package LAT renames Ada.Characters.Latin_1;
   package OPS renames Raven.Database.Operations;
   package DEL renames Raven.Database.Remove;
   package RCU renames Raven.Cmd.Unset;


   ------------------------------
   --  execute_remove_command  --
   ------------------------------
   function execute_remove_command (comline : Cldata) return Boolean
   is
      success     : Boolean;
      rdb         : Database.RDB_Connection;
      toplist     : Pkgtypes.Package_Set.Vector;
      purge_list  : Pkgtypes.Package_Set.Vector;
      purge_order : Purge_Order_Crate.Vector;
   begin
      case OPS.rdb_open_localdb (rdb, Database.installed_packages) is
         when RESULT_OK => null;
         when others => return False;
      end case;

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

      if toplist.Is_Empty then
         if not comline.common_options.quiet then
            Event.emit_message ("No installed packages were selected for removal.");
            return True;
         end if;
      end if;

      DEL.recursive_removal
        (db           => rdb,
         top_packages => toplist,
         purge_list   => purge_list,
         force        => comline.cmd_remove.force_breakage);

      determine_purge_order (purge_list, purge_order);

      --  Show removal list unless --quiet is set
      show_proposed_queue
        (purge_list   => purge_list,
         purge_order  => purge_order,
         behave_quiet => comline.common_options.quiet,
         dryrun       => comline.common_options.dry_run);

      if comline.common_options.dry_run then
         return True;
      end if;

      if not granted_permission_to_proceed (comline.common_options.quiet) then
         return True;
      end if;

   --   Deinstall.deinstall_extracted_package
    --    (installed_package   => Pkgtypes.A_Package,
    --     verify_digest_first => Boolean,
    --     post_report         => TIO.File_Type);

      OPS.rdb_close (rdb);
      return success;
   end execute_remove_command;


   -----------------------------
   --  determine_purge_order  --
   -----------------------------
   procedure determine_purge_order
     (purge_list  : Pkgtypes.Package_Set.Vector;
      purge_order : in out Purge_Order_Crate.Vector)
   is
      tmp_list : Pkgtypes.Text_List.Vector;
      delim    : constant String (1 .. 1) := (others => LAT.HT);
      counter  : Natural := 0;

      procedure scan (Position : Pkgtypes.Package_Set.Cursor)
      is
         n : Text renames Pkgtypes.Package_Set.Element (Position).namebase;
         s : Text renames Pkgtypes.Package_Set.Element (Position).subpackage;
         v : Text renames Pkgtypes.Package_Set.Element (Position).variant;
      begin
         declare
            nsvplus : constant String := USS (n) & '-' & USS (s) & '-' & USS (v) &
              delim & int2str (counter);
         begin
            tmp_list.Append (SUS (nsvplus));
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
      tmp_list.Clear;
      purge_order.Clear;
      purge_list.Iterate (scan'Access);
      Pkgtypes.sorter.Sort (tmp_list);
      tmp_list.Iterate (set_order'Access);
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
   function granted_permission_to_proceed (quiet : Boolean) return Boolean
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
     (purge_list     : Pkgtypes.Package_Set.Vector;
      purge_order    : Purge_Order_Crate.Vector;
      skip_verify    : Boolean;
      quiet          : Boolean)
   is
      tmp_filename  : constant String := Miscellaneous.get_temporary_filename ("remove");
      total_pkgs    : constant Natural := Natural (purge_order.Length);
      deinstall_log : TIO.File_Type;
      pkg_counter   : Natural := 0;

      function progress return String is
      begin
         case total_pkgs is
            when 0 .. 9 =>
               return "[" & int2str (pkg_counter) & "/" & int2str (pkg_counter) & "]";
            when 10 .. 99 =>
               return "[" & zeropad (pkg_counter, 2) & "/" & int2str (pkg_counter) & "]";
            when 100 .. 999 =>
               return "[" & zeropad (pkg_counter, 3) & "/" & int2str (pkg_counter) & "]";
            when 1000 .. 9999 =>
               return "[" & zeropad (pkg_counter, 4) & "/" & int2str (pkg_counter) & "]";
            when others =>
               return "[" & zeropad (pkg_counter, 5) & "/" & int2str (pkg_counter) & "]";
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
      begin
         pkg_counter := pkg_counter + 1;
         if not quiet then
            print_removal_instruction (mypackage);
         end if;
         Deinstall.deinstall_extracted_package
           (installed_package   => mypackage,
            verify_digest_first => not skip_verify,
            post_report         => deinstall_log);
      end remove_installed_package;
   begin
      TIO.Create (deinstall_log, TIO.Out_File, tmp_filename);
      purge_order.Iterate (remove_installed_package'Access);
      TIO.Close (deinstall_log);

      TIO.Open (deinstall_log, TIO.In_File, tmp_filename);
      while not  TIO.End_Of_File (deinstall_log) Loop
         Event.emit_message (TIO.Get_Line (deinstall_log));
      end loop;
      TIO.Close (deinstall_log);

      if Archive.Unix.file_exists (tmp_filename) then
         if not Archive.Unix.unlink_file (tmp_filename) then
            Event.emit_debug (moderate, "Failed to unlink temporary file " & tmp_filename);
         end if;
      end if;
   end remove_packages_in_order;

end Raven.Cmd.Remove;
