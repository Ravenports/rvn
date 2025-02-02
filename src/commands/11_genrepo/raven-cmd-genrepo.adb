--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Text_IO;
with Ada.Direct_IO;
with Ada.Exceptions;
with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Characters.Latin_1;
with GNAT.OS_Lib;

with Archive.Dirent.Scan;
with Archive.Unix;
with Archive.Pack;
with Archive.Unpack;
with ThickUCL.Emitter;
with Ucl;
with Blake_3;
with Raven.Unix;
with Raven.Event;
with Raven.Strings;
with Raven.Miscellaneous;

package body Raven.Cmd.Genrepo is

   package TIO renames Ada.Text_IO;
   package DIR renames Ada.Directories;
   package SIO renames Ada.Streams.Stream_IO;
   package LAT renames Ada.Characters.Latin_1;
   package OSL renames GNAT.OS_Lib;
   package SCN renames Archive.Dirent.Scan;
   package UNX renames Archive.Unix;

   -------------------------------
   --  execute_genrepo_command  --
   -------------------------------
   function execute_genrepo_command (comline : Cldata) return Boolean
   is
      procedure clean_up (filename : String);

      ncpu      : constant MX.CPU_Range := MX.Number_Of_CPUs;
      nocat     : constant String := "The catalog was not created.";
      repo_path : constant String := Strings.USS (comline.common_options.name_pattern);
      quiet     : constant Boolean := comline.common_options.quiet;
      pass_pkey : constant Boolean := not Strings.IsBlank (comline.cmd_genrepo.key_public);
      pass_key  : constant Boolean := not Strings.IsBlank (comline.cmd_genrepo.key_private);
      pass_cmd  : constant Boolean := not Strings.IsBlank (comline.cmd_genrepo.sign_command);
      gen_print : constant Boolean := not Strings.IsBlank (comline.cmd_genrepo.fprint_file);
      catalog   : constant String := repo_path & "/" & CAT_UCL;
      repo_key  : constant String := repo_path & "/" & REPO_PUBKEY;
      include_public_key : Boolean := False;
      include_signature  : Boolean := False;

      procedure clean_up (filename : String) is
      begin
         if DIR.Exists (filename) then
            DIR.Delete_File (filename);
         end if;
      end clean_up;
   begin
      if not analyze_package_files (repo_path, ncpu, quiet, catalog) then
         Event.emit_message (nocat);
         return False;
      end if;

      if pass_cmd then
         if remotely_sign_catalog
           (repo_path, catalog, Strings.USS (comline.cmd_genrepo.sign_command))
         then
            include_public_key := True;
            include_signature := True;
         else
            Event.emit_message ("Invalid response to signing command");
            Event.emit_message ("The generated repository will not be signed.");
         end if;
      end if;

      if pass_pkey then
         DIR.Copy_File
           (Source_Name => Strings.USS (comline.cmd_genrepo.key_public),
            Target_Name => repo_key);
         include_public_key := True;
      end if;

      if pass_key then
         --  sign the hash of the catalog.ucl file
         if not create_signature_file
           (repo_path => repo_path,
            key_path  => Strings.USS (comline.cmd_genrepo.key_private),
            catalog   => catalog)
         then
            return False;
         end if;
         include_signature := True;
      end if;

      if not compress_catalog (repo_path, include_public_key, include_signature) then
         Event.emit_message (nocat);
         return False;
      end if;

      if not create_catalog_digest_file (repo_path) then
         Event.emit_message ("The catalog.sum digest file was not created.");
         return False;
      end if;

      if not quiet then
         if include_signature then
            Event.emit_message ("Cryptographically signed repository created.");
         else
            Event.emit_message ("Repository created.");
         end if;
      end if;

      if include_public_key then
         declare
            hash : Blake_3.blake3_hash_hex;
         begin
            hash := Blake_3.hex (Blake_3.file_digest (repo_key));
            if not quiet then
               Event.emit_message ("Fingerprint: " & hash);
            end if;
            if gen_print then
               declare
                  handle : TIO.File_Type;
                  DQ : constant Character := '"';
                  location : constant String := Strings.USS (comline.cmd_genrepo.fprint_file);
               begin
                  TIO.Create (handle, TIO.Out_File, location);
                  TIO.Put_Line (handle, "function: " & DQ & "blake3" & DQ);
                  TIO.Put_Line (handle, "fingerprint: " & DQ & hash & DQ);
                  TIO.Close (handle);
                  if not quiet then
                     Event.emit_message ("Written to " & location);
                  end if;
               exception
                  when problems : others =>
                     if TIO.Is_Open (handle) then
                        TIO.Close (handle);
                     end if;
                     Event.emit_debug (high_level, "Gen fingerprint file: " &
                                         Ada.Exceptions.Exception_Message (problems));
                     Event.emit_error ("Failed to generate fingerprint file");
               end;
            end if;
         end;
      end if;

      clean_up (repo_key);
      clean_up (catalog);
      clean_up (repo_path & "/" & CAT_SIGNATURE);

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

      subtype Scanner_Range is MX.CPU_Range range 1 .. MAX_SCANNERS;

      type A_Task_Queue is array (Scanner_Range) of string_crate.Vector;
      type A_Task_State is array (Scanner_Range) of Boolean;
      type A_Task_File  is array (Scanner_Range) of TIO.File_Type;
      type A_Task_Count is array (Scanner_Range) of Natural;
      type A_Task_Product is array (Scanner_Range) of Text;
      type progress_increments is (ten, five, one, tenth);

      task_queue      : A_Task_Queue;
      task_files      : A_Task_File;
      task_product    : A_Task_Product;
      task_count      : A_Task_Count := (others => 0);
      finished_task   : A_Task_State := (others => False);
      number_scanners : Natural := Natural (number_cpus);
      total_num       : Natural := 0;
      must_wait       : Boolean := True;
      combined_well   : Boolean := True;
      fragments_good  : Boolean := True;
      show_info       : Boolean := False;
      catalog_handle  : TIO.File_Type;
      last_worker     : Scanner_Range := 1;
      pincrement      : progress_increments := tenth;
      PROGRESS_MIN    : constant Natural := 100;

      procedure create_fragment_files is
      begin
         for lot in 1 .. last_worker loop
            declare
               temp_filename : constant String := Miscellaneous.get_temporary_filename ("genrepo");
            begin
               begin
                  TIO.Create (task_files (lot), TIO.Out_File, temp_filename);
                  task_product (lot) := Strings.SUS (temp_filename);
               exception
                  when others =>
                     Event.emit_error ("Failed to create temporary file " & temp_filename);
                     fragments_good := False;
                     exit;
               end;
            end;
         end loop;
      end create_fragment_files;

      procedure close_fragment_files is
      begin
         for lot in 1 .. last_worker loop
            if TIO.Is_Open (task_files (lot)) then
               TIO.Close (task_files (lot));
            end if;
         end loop;
      end close_fragment_files;

      procedure split_tasks
      is
         procedure filter_rvn_files (position : SCN.dscan_crate.Cursor);
         procedure slice_rvn_files (position : string_crate.Cursor);

         --  parser evaluation already verified repo_path is a directory
         all_files : SCN.dscan_crate.Vector;
         rvn_files : string_crate.Vector;
         tracker   : Natural := 0;
         worklimit : Natural := 0;
         features  : Archive.Unix.File_Characteristics;

         MIN_PACKAGE_COUNT : constant Positive := 240;

         procedure filter_rvn_files (position : SCN.dscan_crate.Cursor)
         is
            item      : Archive.Dirent.Directory_Entity renames SCN.dscan_crate.Element (position);
            filename  : constant String := item.simple_name;
         begin
            if not Strings.trails (filename, extension) then
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
            task_queue (last_worker).Append (string_crate.Element (position));
            tracker := tracker + 1;
            if tracker > worklimit then
               last_worker := last_worker + 1;
               tracker := 0;
            end if;
         end slice_rvn_files;
      begin
         features := Archive.Unix.get_charactistics (repo_path & "/files");
         case features.ftype is
            when archive.directory => null;
            when archive.unsupported =>
               Event.emit_error ("There is no 'files' subdirectory of repository at " & repo_path);
               return;
            when others =>
               Event.emit_error (repo_path & "/files is not a directory");
               return;
         end case;

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

            just_stop   : Boolean := False;
            features    : UNX.File_Characteristics;
            hash        : Blake_3.blake3_hash_hex;

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
               begin
                  operation.open_rvn_archive
                    (rvn_archive   => archive_path,
                     verbosity     => Archive.silent,
                     optional_pipe => Archive.Unix.not_connected);
               exception
                  when failed_open : others =>
                     just_stop := True;
                     Event.emit_error ("Failed to open RVN archive " & archive_path);
                     Event.emit_error
                       ("Details: " & Ada.Exceptions.Exception_Information (failed_open));
                     return;
               end;
               operation.populate_metadata_tree (metatree);
               operation.close_rvn_archive;

               --  Insert size of the package file
               features := UNX.get_charactistics (archive_path);  --  regular file verified
               ThickUCL.insert (metatree, "rvnsize", Ucl.ucl_integer (features.size));

               --  Insert Blake3 sum of the package
               hash := Blake_3.hex (Blake_3.file_digest (archive_path));
               ThickUCL.insert (metatree, "rvndigest", hash);

               ThickUCL.drop_base_keypair (metatree, "directories");
               ThickUCL.drop_base_keypair (metatree, "scripts");
               TIO.Put_Line (task_files (lot), ThickUCL.Emitter.emit_compact_ucl (metatree));
               task_count (lot) := task_count (lot) + 1;

            end scan_rvn_package;
         begin
            Event.emit_debug (low_level, "Started scan task" & lot'Img);
            task_queue (lot).Iterate (scan_rvn_package'Access);
            finished_task (lot) := True;
            Event.emit_debug (low_level, "Completed scan task" & lot'Img);
         exception
            when disaster : others =>
               Event.emit_error
                 ("!!!! CRASH !!!! " & Ada.Exceptions.Exception_Information (disaster));
               Event.emit_stack_trace (disaster);
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
            Event.emit_message ("Scanning" & total_num'Img & " packages for catalog generation...");
            if total_num >= PROGRESS_MIN then
               show_info := True;
               Event.emit_premessage ("  Progress: 0%" & LAT.CR);
            end if;
         end if;
      end execute_scan;

      procedure show_progress
      is
         completed : Natural := 0;
         resint    : Natural;

         procedure print (num_image : String) is
         begin
            Event.emit_premessage ("  Progress: " & num_image & "%" & LAT.CR);
         end print;
      begin
         for z in Scanner_Range loop
            completed := completed + task_count (z);
         end loop;
         case pincrement is
            when ten =>
               resint := ((completed * 10) / total_num) * 10;
            when five =>
               resint := ((completed * 20) / total_num) * 5;
            when one =>
               resint := (completed * 100) / total_num;
            when tenth =>
               resint := ((total_num + completed) * 1000) / total_num;
         end case;
         declare
            raw : constant String := Strings.int2str (resint);
         begin
            case pincrement is
               when ten | five | one =>
                  print (raw);
               when tenth =>
                  --  minimum value is "1000", 4 cols guaranteed.  always drop first column
                  --  if completed = total num, resint = 2000
                  if resint = 2000 then
                     print("100.0");
                  else
                     print (raw (raw'First + 1 .. raw'First + 2) & '.' & raw (raw'Last));
                  end if;
            end case;
         end;
      end show_progress;

   begin
      if number_scanners > Natural (Scanner_Range'Last) then
         number_scanners := Natural (Scanner_Range'Last);
      end if;
      split_tasks;
      if total_num = 0 then
         Event.emit_message ("No RVN package files have been found.");
         return False;
      end if;
      create_fragment_files;
      if not fragments_good then
         return False;
      end if;
      if total_num < 240 then
         pincrement := ten;
      elsif total_num < 800 then
         pincrement := five;
      elsif total_num < 4000 then
         pincrement := one;
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
         if show_info then
            show_progress;
         end if;
      end loop;
      close_fragment_files;

      begin
         TIO.Create (catalog_handle, TIO.Out_File, catalog);
      exception
         when others =>
            Event.emit_error ("Failed to create catalog file " & catalog);
            return False;
      end;

      for z in 1 .. last_worker loop
         declare
            fragment : constant String := Strings.USS (task_product (z));
            fragment_handle : TIO.File_Type;
         begin
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
         end;
      end loop;

      if TIO.Is_Open (catalog_handle) then
         TIO.Close (catalog_handle);
      end if;
      if show_info then
         --  Clear progress (we're done)
         Event.emit_message ("                   ");
      end if;

      return combined_well;
   end analyze_package_files;


   ------------------------
   --  compress_catalog  --
   ------------------------
   function compress_catalog
     (repo_path          : String;
      provided_pubkey    : Boolean;
      provided_signature : Boolean) return Boolean
   is
      whitelist   : constant String := Miscellaneous.get_temporary_filename ("genrepo_compress");
      output_file : constant String := repo_path & "/" & CAT_RVN;
      file_handle : TIO.File_Type;
      dummy_log   : TIO.File_Type;
   begin
      TIO.Create (file_handle, TIO.Out_File, whitelist);
      TIO.Put_Line (file_handle, CAT_UCL);
      if provided_signature then
         TIO.Put_Line (file_handle, CAT_SIGNATURE);
      end if;
      if provided_pubkey then
         TIO.Put_Line (file_handle, REPO_PUBKEY);
      end if;
      TIO.Close (file_handle);

      if not Archive.Pack.integrate
           (top_level_directory => UNX.real_path (repo_path),
            metadata_file       => "",
            manifest_file       => whitelist,
            prefix              => "",
            abi                 => "catalog",
            keyword_dir         => "/",
            output_file         => output_file,
            fixed_timestamp     => 0,
            verbosity           => Archive.silent,
            record_base_libs    => False,
            integrate_log       => dummy_log)
      then
         Event.emit_error ("Failed to integrate catalog to RVN archive");
         DIR.Delete_File (whitelist);
         return False;
      end if;

      DIR.Delete_File (whitelist);
      return True;

   exception
      when others =>
         if TIO.Is_Open (file_handle) then
            TIO.Close (file_handle);
         end if;
         Event.emit_error ("Failed to compress catalog");
         if DIR.Exists (whitelist) then
            DIR.Delete_File (whitelist);
         end if;
         return False;
   end compress_catalog;


   ----------------------------------
   --  create_catalog_digest_file  --
   ----------------------------------
   function create_catalog_digest_file (repo_path : String) return Boolean
   is
      hash     : Blake_3.blake3_hash_hex;
      rvn_file : constant String := repo_path & "/" & CAT_RVN;
      sum_file : constant String := repo_path & "/" & CAT_SUM;
      file_handle : TIO.File_Type;
   begin
      hash := Blake_3.hex (Blake_3.file_digest (rvn_file));

      TIO.Create (file_handle, TIO.Out_File, sum_file);
      TIO.Put_Line (file_handle, hash);
      TIO.Close (file_handle);
      return True;
   exception
      when others =>
         if TIO.Is_Open (file_handle) then
            TIO.Close (file_handle);
         end if;
         Event.emit_error ("Failed to create catalog digest");
         return False;
   end create_catalog_digest_file;


   -----------------------------
   --  create_signature_file  --
   -----------------------------
   function create_signature_file
     (repo_path : String;
      key_path  : String;
      catalog   : String) return Boolean
   is
      c_key_path  : IC.Strings.chars_ptr;
      c_hash_len  : IC.size_t := IC.size_t (Blake_3.blake3_hash'Length);
      c_hash      : array (Blake_3.blake3_hash'Range) of aliased IC.unsigned_char;
      c_capacity  : IC.size_t := 1024;
      c_signature : array (1 .. c_capacity) of aliased IC.unsigned_char := (others => 0);
      c_sig_len   : aliased IC.size_t := 0;
      a_digest    : Blake_3.blake3_hash;
      result      : IC.int;

      use type IC.int;
   begin
      a_digest := Blake_3.file_digest (catalog);
      c_key_path := IC.Strings.New_String (key_path);
      for x in Blake_3.blake3_hash'Range loop
         c_hash (x) := IC.unsigned_char (Character'Pos (a_digest (x)));
      end loop;
      result := Unix.C_Sign_Digest
        (hash      => c_hash (Blake_3.blake3_hash'First)'Access,
         hash_len  => c_hash_len,
         key_path  => c_key_path,
         signature => c_signature (1)'Access,
         sig_cap   => c_capacity,
         sig_len   => c_sig_len'Access);

      IC.Strings.Free (c_key_path);
      if result /= 0 then
         Event.emit_debug (high_level, "Digest failed, RC =" & result'Img);
         Event.emit_error ("Failed to calculate signature.");
         return False;
      end if;

      declare
         type A_Signature is array (1 .. c_sig_len) of IC.unsigned_char;
         type A_Block is
            record
               signature : A_Signature;
            end record;
         block : A_Block;
         for block'alignment use 8;
         ndx_handle : SIO.File_Type;
         ndx_stmaxs : SIO.Stream_Access;
      begin
         block.signature := A_Signature (c_signature (1 .. c_sig_len));

         SIO.Create (File => ndx_handle,
                     Mode => SIO.Out_File,
                     Name => repo_path & "/" & CAT_SIGNATURE);
         ndx_stmaxs := SIO.Stream (ndx_handle);
         A_Block'Output (ndx_stmaxs, block);
         SIO.Close (ndx_handle);
      exception
         when problem : others =>
            Event.emit_debug (high_level, Ada.Exceptions.Exception_Message (problem));
            Event.emit_error ("Failed to write signature file.");
            return False;
      end;

      return True;
   end create_signature_file;


   -------------------------
   --  slurp_sign_output  --
   -------------------------
   function slurp_sign_output (path : String) return String
   is
      filesize : Natural := Natural (DIR.Size (path));
   begin
      if filesize = 0 or else filesize > 4096 then
         return "INVALID";
      end if;

      declare
         subtype File_String    is String (1 .. filesize);
         package File_String_IO is new Ada.Direct_IO (File_String);

         handle   : File_String_IO.File_Type;
         contents : File_String;
      begin
         File_String_IO.Open  (handle, File_String_IO.In_File, path);
         File_String_IO.Read  (handle, contents);
         File_String_IO.Close (handle);

         return contents;
      exception
         when problems : others =>
            if File_String_IO.Is_Open (handle) then
               File_String_IO.Close (handle);
            end if;
            Event.emit_debug (high_level, "slurp_sign_output: " &
                                Ada.Exceptions.Exception_Message (problems));
            return "";
      end;
   end slurp_sign_output;


   ---------------------
   --  generate_file  --
   ---------------------
   procedure generate_file (path : String; contents  : String)
   is
      subtype file_contents is String (1 .. contents'Length);
      package Blitz is new Ada.Direct_IO (file_contents);

      out_handle : Blitz.File_Type;
   begin
      Blitz.Create (out_handle, Blitz.Out_File, path);
      if contents'Length > 0 then
         Blitz.Write (out_handle, file_contents (contents));
      end if;
      Blitz.Close (out_handle);
   exception
      when others =>
         Event.emit_error ("Failed to create " & path);
   end generate_file;


   --------------------------------
   --  construct_remote_command  --
   --------------------------------
   function construct_remote_command
     (template  : String;
      digest    : String) return String
   is
      braces : constant String := "{}";
      braces_found : Boolean := Strings.contains (template, braces);
      result : Text;
   begin
      if not braces_found then
         return template & " " & digest;
      end if;
      result := Strings.SUS (template);
      loop
         exit when not braces_found;
         result := Strings.replace_substring (result, braces, digest);
         braces_found := Strings.contains (result, braces);
      end loop;
      return Strings.USS (result);
   end construct_remote_command;


   -----------------------------
   --  remotely_sign_catalog  --
   -----------------------------
   function remotely_sign_catalog
     (repo_path : String;
      catalog   : String;
      scommand  : String) return Boolean
   is
      Args    : OSL.Argument_List_Access;
      digest  : Blake_3.blake3_hash_hex;
      result  : Boolean := False;
      success : Boolean;
      retcode : Integer;
      signout : constant String := Miscellaneous.get_temporary_filename ("genrepo_sign");
   begin
      digest := Blake_3.hex (Blake_3.file_digest (catalog));
      Args := OSL.Argument_String_To_List (construct_remote_command(scommand, digest));
      OSL.Spawn
        (Program_Name => Args (Args'First).all,
         Args         => Args (Args'First + 1 .. Args'Last),
         Output_File  => signout,
         Success      => success,
         Return_Code  => retcode,
         Err_To_Out   => True);
      OSL.Free (Args);

      Event.emit_debug (moderate, "spawn retcode =" & retcode'Img);
      Event.emit_debug (moderate, "spawn success =" & success'Img);
      --  Don't check success, it doesn't mean what you'd think (returns 0 when command DNE)
      if retcode /= 0 then
         Event.emit_debug (high_level, "remote sign command spawn return code:" & retcode'Img);
         if DIR.Exists (signout) then
            DIR.Delete_File (signout);
         end if;
         return False;
      end if;

      declare
         index_signature : Natural;
         index_cert      : Natural;
         index_end       : Natural;
         sign_output     : constant String := slurp_sign_output (signout);
         LF              : constant Character := Character'Val (10);
         file_pubkey     : constant String := repo_path & "/" & REPO_PUBKEY;
         file_signature  : constant String := repo_path & "/" & CAT_SIGNATURE;
      begin
         index_signature := Strings.start_index (sign_output, "SIGNATURE" & LF);
         index_cert      := Strings.start_index (sign_output, LF & "CERT" & LF);
         index_end       := Strings.start_index (sign_output, LF & "END" & LF);

         if index_signature > 0 and then
           index_cert > 0 and then
           index_end > 0
         then
            generate_file (file_signature, sign_output (index_signature + 10 .. index_cert - 1));
            generate_file (file_pubkey, sign_output (index_cert + 6 .. index_end));
            result := True;
         end if;
      end;

      if DIR.Exists (signout) then
         DIR.Delete_File (signout);
      end if;

      return result;

   end remotely_sign_catalog;

end Raven.Cmd.Genrepo;
