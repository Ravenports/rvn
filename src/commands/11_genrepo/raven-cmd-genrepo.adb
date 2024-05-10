--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Text_IO;
with Ada.Directories;
with Archive.Dirent.Scan;
with Archive.Unix;
with Archive.Unpack;
with ThickUCL.Emitter;
with Raven.Strings;
with Raven.Event;
with Raven.Miscellaneous;

package body Raven.Cmd.Genrepo is

   package TIO renames Ada.Text_IO;
   package DIR renames Ada.Directories;
   package SCN renames Archive.Dirent.Scan;
   package UNX renames Archive.Unix;

   -------------------------------
   --  execute_genrepo_command  --
   -------------------------------
   function execute_genrepo_command (comline : Cldata) return Boolean
   is
      ncpu      : constant MX.CPU_Range := MX.Number_Of_CPUs;
      nocat     : constant String := "The catalog was not created.";
      repo_path : constant String := Strings.USS (comline.common_options.name_pattern);
      quiet     : constant Boolean := comline.common_options.quiet;
      catalog   : constant String := repo_path & "/catalog.ucl";
   begin
      if not analyze_package_files (repo_path, ncpu, quiet, catalog) then
         Event.emit_message (nocat);
         return False;
      end if;
      return True;
   end execute_genrepo_command;


   -----------------------------
   --  analyze_package_files  --
   -----------------------------
   function analyze_package_files
     (repo_path   : String;
      number_cpus : MX.CPU_Range;
      quiet       : Boolean;
      catalog     : String) return Boolean
   is
      --  Regardless of the number of CPUs, limit the number of scanners to MAX_SCANNERS (16)
      --  If number of packages is less than 240, then use a single scanner

      procedure split_tasks;
      procedure execute_scan;

      subtype Scanner_Range is MX.CPU_Range range 1 .. MAX_SCANNERS;

      type A_Task_Queue is array (Scanner_Range) of string_crate.Vector;
      type A_Task_State is array (Scanner_Range) of Boolean;
      type A_Task_Product is array (Scanner_Range) of Text;

      task_queue      : A_Task_Queue;
      task_product    : A_Task_Product;
      finished_task   : A_Task_State := (others => False);
      number_scanners : Natural := Natural (number_cpus);
      total_num       : Natural := 0;
      must_wait       : Boolean := True;
      combined_well   : Boolean := True;
      catalog_handle  : TIO.File_Type;

      procedure split_tasks
      is
         procedure filter_rvn_files (position : SCN.dscan_crate.Cursor);
         procedure slice_rvn_files (position : string_crate.Cursor);

         --  parser evaluation already verified repo_path is a directory
         all_files : SCN.dscan_crate.Vector;
         rvn_files : string_crate.Vector;
         tracker   : Natural := 0;
         worklimit : Natural := 0;
         worker    : Scanner_Range := 1;

         MIN_PACKAGE_COUNT : constant Positive := 240;

         procedure filter_rvn_files (position : SCN.dscan_crate.Cursor)
         is
            item      : Archive.Dirent.Directory_Entity renames SCN.dscan_crate.Element (position);
            filename  : constant String := item.simple_name;
         begin
            if not Strings.trails (filename, ".rvn") then
               return;
            end if;

            declare
               item_path : constant String := item.full_path;
               features  : constant UNX.File_Characteristics := UNX.get_charactistics (item_path);
            begin
               --  Rvn files are exclusively regular files
               case features.ftype is
                  when Archive.regular =>
                     rvn_files.Append (Strings.SUS (filename));
                  when others =>
                     null;
               end case;
            end;
         end filter_rvn_files;

         procedure slice_rvn_files (position : string_crate.Cursor)
         is
            use type MX.CPU_Range;
         begin
            task_queue (worker).Append (string_crate.Element (position));
            tracker := tracker + 1;
            if tracker >= worklimit then
               worker := worker + 1;
               tracker := 0;
            end if;
         end slice_rvn_files;
      begin
         SCN.scan_directory (repo_path & "/files", all_files);
         all_files.Iterate (filter_rvn_files'Access);
         total_num := Natural (rvn_files.Length);
         if total_num < MIN_PACKAGE_COUNT then
            worklimit := total_num + 1;
         else
            worklimit := Natural ((total_num + number_scanners - 1) / number_scanners);
         end if;
         rvn_files.Iterate (slice_rvn_files'Access);
      end split_tasks;

      procedure execute_scan
      is
         task type scan (lot : Scanner_Range);

         task body scan
         is
            procedure scan_rvn_package (position : string_crate.Cursor);

            file_handle : TIO.File_Type;
            created     : Boolean := False;
            just_stop   : Boolean := False;

            procedure scan_rvn_package (position : string_crate.Cursor)
            is
               rvn_filename : constant String := Strings.USS (string_crate.Element (position));
               metatree     : ThickUCL.UclTree;
               operation    : Archive.Unpack.Darc;
               archive_path : constant String := repo_path & "/files/" & rvn_filename;
            begin
               if just_stop then
                  --  Experienced a problem with file creation, let the loop run out
                  return;
               end if;

               Event.emit_debug (high_level, "Scanning " & rvn_filename);
               if not created then
                  begin
                     TIO.Create (file_handle, TIO.Out_File, Strings.USS (task_product (lot)));
                     created := True;
                  exception
                     when others =>
                        just_stop := False;
                        Event.emit_error
                          ("Failed to create temporary file " & rvn_filename);
                  end;
               end if;

               operation.open_rvn_archive
                 (rvn_archive   => archive_path,
                  verbosity     => Archive.silent,
                  optional_pipe => Archive.Unix.not_connected);
               operation.populate_metadata_tree (metatree);
               operation.close_rvn_archive;

               --  TODO: filter out files/directories/scripts
               TIO.Put_Line (file_handle, ThickUCL.Emitter.emit_compact_ucl (metatree));

            end scan_rvn_package;
         begin
            Event.emit_debug (low_level, "Started scan task" & lot'Img);
            task_product (lot) := Strings.SUS (Miscellaneous.get_temporary_filename ("genrepo"));
            task_queue (lot).Iterate (scan_rvn_package'Access);
            if TIO.Is_Open (file_handle) then
               TIO.Close (file_handle);
            end if;
            finished_task (lot) := True;
            Event.emit_debug (low_level, "Completed scan task" & lot'Img);
         end scan;

         scan_01 : scan (lot => 1);
         scan_02 : scan (lot => 2);
         scan_03 : scan (lot => 3);
         scan_04 : scan (lot => 4);
         scan_05 : scan (lot => 5);
         scan_06 : scan (lot => 6);
         scan_07 : scan (lot => 7);
         scan_08 : scan (lot => 8);
         scan_09 : scan (lot => 9);
         scan_10 : scan (lot => 10);
         scan_11 : scan (lot => 11);
         scan_12 : scan (lot => 12);
         scan_13 : scan (lot => 13);
         scan_14 : scan (lot => 14);
         scan_15 : scan (lot => 15);
         scan_16 : scan (lot => 16);
      begin
         Event.emit_debug (moderate, "All tasks spawned");
         if not quiet then
            Event.emit_message ("Scanning packages for catalog generation...");
         end if;
      end execute_scan;

   begin
      if number_scanners > Natural (Scanner_Range'Last) then
         number_scanners := Natural (Scanner_Range'Last);
      end if;
      split_tasks;
      if total_num = 0 then
         Event.emit_message ("No RVN package files have been found.");
         return False;
      end if;

      execute_scan;
      while must_wait loop
         delay 0.25;
         must_wait := False;
         for z in Scanner_Range loop
            if not finished_task (z) then
               must_wait := True;
            end if;
         end loop;
      end loop;

      begin
         TIO.Create (catalog_handle, TIO.Out_File, catalog);
      exception
         when others =>
            Event.emit_error ("Failed to create catalog file " & catalog);
            return False;
      end;

      for z in Scanner_Range loop
         declare
            fragment : constant String := Strings.USS (task_product (z));
            fragment_handle : TIO.File_Type;
         begin
            if DIR.Exists (fragment) then
               begin
                  TIO.Open (fragment_handle, TIO.In_File, fragment);
                  while not TIO.End_Of_File (fragment_handle) loop
                     TIO.Put_Line (catalog_handle, TIO.Get_Line (fragment_handle));
                  end loop;
               exception
                  when others =>
                     Event.emit_error ("Failed to open fragment" & z'Img & " file " & fragment);
                     combined_well := False;
               end;
               if TIO.Is_Open (fragment_handle) then
                  TIO.Close (fragment_handle);
               end if;

               begin
                  DIR.Delete_File (fragment);
               exception
                  when others =>
                     Event.emit_error ("Failed to delete fragment" & z'Img & " file " & fragment);
               end;
            end if;
         end;
      end loop;

      if TIO.Is_Open (catalog_handle) then
         TIO.Close (catalog_handle);
      end if;

      return combined_well;
   end analyze_package_files;

end Raven.Cmd.Genrepo;
