--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Text_IO;
with Ada.Characters.Latin_1;
with Raven.Event;
with Raven.Fetch;
with Raven.Metadata;
with Raven.Cmd.Unset;
with Raven.Repository;
with Raven.Database.CommonSQL;
with Raven.Strings;  use Raven.Strings;
with Raven.Pkgtypes; use Raven.Pkgtypes;
with Archive.Unix;
with Blake_3;

package body Raven.Database.Fetch is

   package LAT renames Ada.Characters.Latin_1;
   package RCU renames Raven.Cmd.Unset;
   package DLF renames Raven.Fetch;


   -------------------------------------
   --  retrieve_remote_file_metadata  --
   -------------------------------------
   procedure retrieve_remote_file_metadata
     (db           : RDB_Connection;
      sql          : String;
      bind_one     : String;
      like_match   : Boolean;
      remote_files : in out Remote_Files_Set.Map)
   is
      func     : constant String := "retrieve_remote_file_metadata";
      new_stmt : SQLite.thick_stmt;
   begin
      if not SQLite.prepare_sql (db.handle, sql, new_stmt) then
         CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func, sql);
         return;
      end if;
      if bind_one /= "" then
         if like_match then
            SQLite.bind_string (new_stmt, 1, bind_one & '%');
         else
            SQLite.bind_string (new_stmt, 1, bind_one);
         end if;
      end if;
      debug_running_stmt (new_stmt);


      loop
         case SQLite.step (new_stmt) is
            when SQLite.row_present =>
               declare
                  myrec : A_Remote_File;
                  b3dig : constant String := SQLite.retrieve_string (new_stmt, 3);
                  key   : constant short_digest := b3dig (b3dig'First .. b3dig'First + 9);
               begin
                  if not remote_files.Contains (key) then
                     myrec.nsvv := SUS (SQLite.retrieve_string (new_stmt, 0));
                     myrec.flatsize := Package_Size (SQLite.retrieve_integer (new_stmt, 1));
                     myrec.rvnsize  := Package_Size (SQLite.retrieve_integer (new_stmt, 2));
                     remote_files.Insert (key, myrec);
                  end if;
               end;
            when SQLite.something_else =>
               CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                            SQLite.get_expanded_sql (new_stmt));
               exit;
            when SQLite.no_more_data => exit;
         end case;
      end loop;
      SQLite.finalize_statement (new_stmt);

   end retrieve_remote_file_metadata;


   --------------------------
   --  rvn_core_retrieval  --
   --------------------------
   function rvn_core_retrieval
     (db           : RDB_Connection;
      patterns     : Pkgtypes.Text_List.Vector;
      behave_exact : Boolean;
      behave_cs    : Boolean;
      behave_quiet : Boolean;
      behave_yes   : Boolean;
      select_all   : Boolean;
      select_deps  : Boolean;
      destination  : String;
      single_repo  : String) return Boolean
   is
      basesql : constant String :=
        "SELECT namebase ||'-'|| subpackage ||'-'|| variant ||'-'|| version as nsvv, " &
        "flatsize, rvnsize, rvndigest FROM packages";
      download_list : Remote_Files_Set.Map;
      num_patterns  : constant Natural := Natural (patterns.Length);
      leading_match : constant Boolean := not behave_exact and then not behave_cs;
      send_to_cache : constant Boolean := destination = "";
      download_dir  : constant String := translate_destination (destination);
      download_order : Text_List.Vector;

      function extended_sql return String is
      begin
         if behave_exact then
            return basesql & " WHERE nsvv = ?";
         elsif behave_cs then
            return basesql &" WHERE nsvv GLOB ?";
         end if;
         return basesql & " WHERE nsvv LIKE ?";
      end extended_sql;
   begin
      if select_deps then
         Event.emit_error ("--dependencies not yet supported");
         return False;
      end if;

      if select_all then
        retrieve_remote_file_metadata (db, basesql, "", False, download_list);
      else
         for pattx in 0 .. num_patterns - 1 loop
            retrieve_remote_file_metadata
              (db           => db,
               sql          => extended_sql,
               bind_one     => USS (patterns.Element (pattx)),
               like_match   => leading_match,
               remote_files => download_list);
         end loop;
      end if;

      if download_list.Is_Empty then
         if not behave_quiet then
            Event.emit_message
              ("No files matching the given pattern(s) have been found in the catalog.");
            return False;
         end if;
      end if;

      prune_remote_files_list (download_list, download_dir);

      if download_list.Is_Empty then
         if not behave_quiet then
            Event.emit_message
              ("The latest versions of the requested packages are already available locally.");
            return True;
         end if;
      end if;

      sort_queue (download_list, download_order);
      show_proposed_queue (download_list, download_order, behave_quiet);

      if not Archive.Unix.file_is_writable (download_dir) then
         Event.emit_message (LAT.LF & "The download directory, " & download_dir &
                               ", is not writable by this user.");
         Event.emit_message ("Switch to the superuser or choose a different output directory.");
         return False;
      end if;

      if not granted_permission_to_proceed (behave_quiet) then
         return True;
      end if;

      return download_packages
        (download_list, download_order, behave_quiet, send_to_cache, download_dir, single_repo);

   end rvn_core_retrieval;


   -------------------------------
   --  prune_remote_files_list  --
   -------------------------------
   procedure prune_remote_files_list
     (remote_files  : in out Remote_Files_Set.Map;
      destination   : String)
   is
      function set_destination return String is
      begin
         if destination /= "" then
            return destination;
         end if;
         return RCU.config_setting (RCU.CFG.cachedir);
      end set_destination;

      purge_list : Text_List.Vector;
      filedir    : constant String := set_destination;

      procedure check_for_file (Position : Remote_Files_Set.Cursor)
      is
         full_path : constant String := filedir & "/" &
           USS (Remote_Files_Set.Element (Position).nsvv) & extension;
      begin
         if Archive.Unix.file_exists (full_path) then
            purge_list.Append (SUS (Remote_Files_Set.Key (Position)));
         end if;
      end check_for_file;

      procedure drop_element (Position : Text_List.Cursor) is
         digestkey : short_digest := USS (Text_List.Element (Position));
      begin
         remote_files.Delete (digestkey);
      end drop_element;
   begin

      remote_files.Iterate (check_for_file'Access);
      purge_list.Iterate (drop_element'Access);
   end prune_remote_files_list;


   ------------------
   --  sort_queue  --
   ------------------
   procedure sort_queue
     (remote_files   : Remote_Files_Set.Map;
      download_order : in out Text_List.Vector)
   is
      temp_storage : Text_List.Vector;
      delim : String (1 .. 1) := (others => LAT.HT);

      procedure push_to_temp (Position : Remote_Files_Set.Cursor)
      is
         payload : Text := Remote_Files_Set.Element (Position).nsvv;
      begin
         SU.Append (payload, delim);
         SU.Append (payload, Remote_Files_Set.Key (Position));
         temp_storage.Append (payload);
      end push_to_temp;

      procedure push (Position : Text_List.Cursor)
      is
         rvn_key : constant String := part_2 (USS (Text_List.Element (Position)), delim);
      begin
         download_order.Append (SUS (rvn_key));
      end push;
   begin
      download_order.clear;
      remote_files.Iterate(push_to_temp'Access);
      sorter.Sort (temp_storage);
      temp_storage.Iterate (Push'Access);
   end sort_queue;


   ---------------------------
   --  show_proposed_queue  --
   ---------------------------
   procedure show_proposed_queue
     (remote_files   : Remote_Files_Set.Map;
      download_order : Pkgtypes.Text_List.Vector;
      behave_quiet   : Boolean)
   is
      --  Display format
      --   6 chars: right-padded 5 spaces counter, plus + space
      --  57 chars: max to displayed.  If > 55, truncate at 54 and add "*"
      --   1 chars: space
      --   8 chars: human readable, right pad
      --   1 chars: space
      --   6 chars: left-pad, "  0.0%" .. "100.0%"
      --  --------------
      --  79 chars

      total_flatsize : Package_Size := 0;
      total_rvn_size : Package_Size := 0;
      counter        : Natural := 0;

      procedure combine_sizes (Position : Remote_Files_Set.Cursor) is
      begin
         total_flatsize := total_flatsize + Remote_Files_Set.Element (Position).flatsize;
         total_rvn_size := total_rvn_size + Remote_Files_Set.Element (Position).rvnsize;
      end combine_sizes;

      procedure display_line (Position : Text_List.Cursor)
      is
         digkey : constant short_digest := USS (Text_List.Element (Position));
         myrec  : A_Remote_File renames remote_files.Element (digkey);
         full_line : String (1 .. 79) := (others => ' ');
         IEC   : constant String := Metadata.human_readable_size (int64 (myrec.rvnsize));
         bname : constant String := USS (myrec.nsvv);
      begin
         counter := counter + 1;
         full_line (1 .. 5) := format_download_order (counter);
         if bname'Length > 57 then
            full_line (7 .. 62) := bname (bname'First .. bname'First + 55);
            full_line (63) := '*';
         else
            full_line (7 .. 6 + bname'Length) := bname;
         end if;
         if myrec.rvnsize < 5_121 then
            declare
               bytes : constant String := int2str (Natural (myrec.rvnsize)) & " B  ";
            begin
               full_line (65 .. 72) := pad_left (bytes, 8);
            end;
         else
            full_line (65 .. 72) := pad_left (IEC, 8);
         end if;
         full_line (74 .. 79) := pad_left (percentage (myrec.rvnsize, total_rvn_size), 6);
         Event.emit_message (full_line);
      end display_line;

   begin
      if behave_quiet then
         return;
      end if;
      remote_files.Iterate (combine_sizes'Access);

      Event.emit_message ("The following packages will be downloaded:" & LAT.LF);
      download_order.Iterate (display_line'Access);
      Event.emit_message (LAT.LF & "Total data to download: " &
                           Metadata.human_readable_size (int64 (total_rvn_size)));
      Event.emit_message ("Disk space required to install these packages: " &
                           Metadata.human_readable_size (int64 (total_flatsize)));

   end show_proposed_queue;


   ------------------
   --  percentage  --
   ------------------
   function percentage (numerator, denominator : Pkgtypes.Package_Size) return string
   is
      answer : Float;
      float_num : constant Float := Float (numerator);
      float_den : constant Float := Float (denominator);
      answer_int : Integer;
   begin
      answer := (1000.0 * float_num / float_den) + 0.5;
      answer_int := Integer (float'Floor (answer));
      declare
         s : constant String := int2str (answer_int);
      begin
         case answer_int is
            when 0 .. 9 => return "0." & int2str(answer_int) & "%";
            when 10 .. 1000 => return s (s'First .. S'Last - 1) & "." & s(s'Last) & "%";
            when others => return "!range";
         end case;
      end;
   end percentage;


   -----------------------------
   --  format_download_order  --
   -----------------------------
   function format_download_order (counter : Natural) return String is
   begin
      if counter < 10_000 then
         return pad_left (int2str (counter), 4) & '.';
      end if;
      return pad_left (int2str (counter), 5);  --  truncates from front at 100,000+
   end format_download_order;


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

      Event.emit_message (LAT.LF & "Proceed with fetching packages? [y/n]: ");
      Ada.Text_IO.Get_Immediate (cont);
      case cont is
         when 'Y' | 'y' => return True;
         when others => return False;
      end case;
   end granted_permission_to_proceed;


   -------------------------
   --  download_packages  --
   -------------------------
   function download_packages
     (remote_files   : Remote_Files_Set.Map;
      download_order : Pkgtypes.Text_List.Vector;
      behave_quiet   : Boolean;
      send_to_cache  : Boolean;
      destination    : String;
      single_repo    : String) return Boolean
   is
      mirrors : Repository.A_Repo_Config_Set;
      success : Boolean := True;
      postlog : Text := SU.Null_Unbounded_String;
   begin
      Repository.load_repository_configurations (mirrors, single_repo);
      if mirrors.search_order.Is_Empty then
         Event.emit_error ("Fetch command: No repositories are configured.");
         return False;
      end if;

      declare
         repo : Repository.A_Repo_Config renames
           mirrors.repositories.Element (mirrors.search_order.Element (0));

         counter     : Natural := 0;
         total_files : Natural := Natural (download_order.Length);

         procedure retrieve_rvn_file (Position : Text_List.Cursor)
         is
            digkey : constant short_digest := USS (Text_List.Element (Position));
            myrec  : A_Remote_File renames remote_files.Element (digkey);
         begin
            counter := counter + 1;
            case download_package (remote_url    => repo.url,
                                   remote_proto  => repo.protocol,
                                   remote_file   => myrec,
                                   digest10      => digkey,
                                   destination   => destination,
                                   send_to_cache => send_to_cache,
                                   behave_quiet  => behave_quiet,
                                   file_counter  => counter,
                                   total_files   => total_files)
            is
               when verified_download => null;
               when failed_download =>
                  success := False;
                  SU.Append (postlog, "Failed download: " &
                               USS (myrec.nsvv) & extension & LAT.LF);
               when failed_verification =>
                  success := False;
                  SU.Append (postlog, "Checksum verification failed: " &
                               USS (myrec.nsvv) & extension & LAT.LF);
            end case;
         end retrieve_rvn_file;
      begin
         download_order.Iterate (retrieve_rvn_file'Access);
      end;

      if not success and then not behave_quiet then
         Event.emit_error (LAT.LF & USS (postlog));
      end if;

      return Success;

   end download_packages;


   ------------------------
   --  download_package  --
   ------------------------
   function download_package
     (remote_url     : Text;
      remote_proto   : IP_support;
      remote_file    : A_Remote_File;
      digest10       : short_digest;
      destination    : String;
      send_to_cache  : Boolean;
      behave_quiet   : Boolean;
      file_counter   : Natural;
      total_files    : Natural) return download_result
   is
      full_line : String (1 .. 75) := (others => ' ');
      rf_url : constant String := USS (remote_url) & "/files/" & USS (remote_file.nsvv) & extension;
      dnlink : constant String := destination & "/" & USS (remote_file.nsvv) & extension;
      dnfile : constant String := destination & "/" & USS (remote_file.nsvv) & "~"& digest10 &
                                  extension;
      status_download : constant String := "DOWN";
      status_okay     : constant String := LAT.BS & LAT.BS & LAT.BS & LAT.BS & "[ok]";
      status_verify   : constant String := LAT.BS & LAT.BS & LAT.BS & LAT.BS & "VSUM";
      status_bad_down : constant String := LAT.BS & LAT.BS & LAT.BS & LAT.BS & "FAIL";
      status_bad_sum  : constant String := LAT.BS & LAT.BS & LAT.BS & LAT.BS & "FSUM";
      fetres : DLF.fetch_result;

      function progress return String is
      begin
         case total_files is
            when 0 .. 9 =>
               return "[" & int2str (file_counter) & "/" & int2str (total_files) & "]";
            when 10 .. 99 =>
               return "[" & zeropad (file_counter, 2) & "/" & int2str (total_files) & "]";
            when 100 .. 999 =>
               return "[" & zeropad (file_counter, 3) & "/" & int2str (total_files) & "]";
            when 1000 .. 9999 =>
               return "[" & zeropad (file_counter, 4) & "/" & int2str (total_files) & "]";
            when others =>
               return "[" & zeropad (file_counter, 5) & "/" & int2str (total_files) & "]";
         end case;
      end progress;

      procedure populate_filename (prefix_size : Natural)
      is
         max_size : constant Natural := 66 - prefix_size - 2;
         filename : constant String := USS (remote_file.nsvv);
         offset   : constant Natural := prefix_size + 1;
         start    : constant Natural := full_line'First + offset;
         max_fn   : constant Natural := filename'first + 63 - offset;
      begin
         if filename'Length < max_size then
            full_line (start .. start + filename'Length - 1) := filename;
         else
            full_line (start .. 64) := filename (filename'First .. max_fn);
            full_line (65) := '*';
         end if;
      end populate_filename;

      procedure populate_filesize
      is
         IEC : constant String := Metadata.human_readable_size (int64 (remote_file.rvnsize));
      begin
         if remote_file.rvnsize < 5_121 then
            declare
               bytes : constant String := int2str (Natural (remote_file.rvnsize)) & " B  ";
            begin
               full_line (67 .. 74) := pad_left (bytes, 8);
            end;
         else
            full_line (67 .. 74) := pad_left (IEC, 8);
         end if;
      end populate_filesize;

   begin
      declare
         fragment : constant String := progress;
         fragsize : constant Natural := fragment'Length;
      begin
         full_line (1 .. fragsize) := fragment;
         populate_filename (fragsize);
         populate_filesize;
         Event.emit_premessage (full_line);
         Event.emit_premessage (status_download);

         if send_to_cache then
            fetres := DLF.download_file (remote_file_url => rf_url,
                                         etag_file       => "",
                                         downloaded_file => dnfile,
                                         remote_repo     => True,
                                         remote_protocol => remote_proto);
            case fetres is
               when DLF.cache_valid | DLF.file_downloaded  =>
                  Event.emit_premessage (status_verify);
                  if verify_checksum (dnfile, digest10) then
                     Event.emit_message (status_okay);
                     if Archive.Unix.file_exists (dnlink) then
                        if not Archive.Unix.unlink_file (dnlink) then
                           Event.emit_debug
                             (moderate, "Failed to unlink expected symlink " & dnlink);
                        end if;
                     end if;
                     if not Archive.Unix.create_symlink (dnfile, dnlink) then
                        Event.emit_debug
                          (high_level, "Failed to create symlink " & dnlink & " to " & dnfile);
                     end if;
                     return verified_download;
                  end if;
                  Event.emit_message (status_bad_sum);
                  if not Archive.Unix.unlink_file (dnfile) then
                     Event.emit_debug (moderate, "Failed to unlink corrupted download: " & dnfile);
                  end if;
                  return failed_verification;
               when DLF.retrieval_failed =>
                  Event.emit_debug (high_level, "Failed to download " & rf_url & " to " & dnfile);
                  Event.emit_message (status_bad_down);
                  return failed_download;
            end case;
         else
            fetres := DLF.download_file (remote_file_url => rf_url,
                                         etag_file       => "",
                                         downloaded_file => dnlink,
                                         remote_repo     => True,
                                         remote_protocol => remote_proto);
            case fetres is
               when DLF.cache_valid | DLF.file_downloaded  =>
                  Event.emit_premessage (status_verify);
                  if verify_checksum (dnlink, digest10) then
                     Event.emit_message (status_okay);
                     return verified_download;
                  end if;
                  Event.emit_message (status_bad_sum);
                  if not Archive.Unix.unlink_file (dnlink) then
                     Event.emit_debug (moderate, "Failed to unlink corrupted download: " & dnlink);
                  end if;
                  return failed_verification;
               when DLF.retrieval_failed =>
                  Event.emit_debug (high_level, "Failed to download " & rf_url & " to " & dnlink);
                  Event.emit_message (status_bad_down);
                  return failed_download;
            end case;
         end if;
      end;
   end download_package;

   -----------------------------
   --  translate_destination  --
   -----------------------------
   function translate_destination (destination : String) return String is
   begin
      if destination /= "" then
         return destination;
      end if;
      return RCU.config_setting (RCU.CFG.cachedir);
   end translate_destination;


   -----------------------
   --  verify_checksum  --
   -----------------------
   function verify_checksum (file_url : String; digest10 : short_digest) return Boolean
   is
      features : Archive.Unix.File_Characteristics;
      b3sum    : Blake_3.blake3_hash_hex;
   begin
      features := Archive.Unix.get_charactistics (file_url);
      case features.ftype is
         when Archive.regular => null;
         when others =>
            Event.emit_debug (moderate, "verify_checksum: not a regular file: " & file_url);
            return False;
      end case;
      b3sum := Blake_3.hex (Blake_3.file_digest (file_url));
      return leads (b3sum, digest10);
   end verify_checksum;

end Raven.Database.Fetch;
