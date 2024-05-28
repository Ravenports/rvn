--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Event;
with Raven.Cmd.Unset;
with Raven.Metadata;
with Raven.Database.CommonSQL;
with Raven.Strings;  use Raven.Strings;
with Raven.Pkgtypes; use Raven.Pkgtypes;
with Archive.Unix;

package body Raven.Database.Fetch is

   package RCU renames Raven.Cmd.Unset;

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
            SQLite.bind_string (new_stmt, 1, bind_one);
         else
            SQLite.bind_string (new_stmt, 1, bind_one & '%');
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
      destination  : String) return Boolean
   is
      basesql : constant String :=
        "SELECT namebase ||-|| subpackage ||-|| variant ||-|| version as nsvv, " &
        "flatsize, rvnsize, rvndigest FROM packages";
      download_list : Remote_Files_Set.Map;
      num_patterns  : constant Natural := Natural (patterns.Length);
      leading_match : constant Boolean := not behave_exact and then not behave_cs;
      download_order : Text_List.Vector;

      function extended_sql return String is
      begin
         if behave_exact then
            return basesql & " AND nsvv = ?";
         elsif behave_cs then
            return basesql &" AND nsvv GLOB ?";
         end if;
         return basesql & " AND nsvv LIKE ?";
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

      prune_remote_files_list (download_list, destination);

      if download_list.Is_Empty then
         if not behave_quiet then
            Event.emit_message
              ("The latest versions of the requested packages are already available locally.");
            return True;
         end if;
      end if;

      sort_queue (download_list, download_order);
      show_proposed_queue (download_list, download_order, behave_quiet);


      return False;
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
      delim : String (1 .. 1) := (others => Character'Val (9));

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
      LF             : constant Character := Character'Val(10);

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
         full_line (1 .. 5) := pad_left (int2str (counter), 5);
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

      Event.emit_notice ("The following packages will be downloaded:" & LF);
      download_order.Iterate (display_line'Access);
      Event.emit_notice (LF & "Total data to download: " &
                           Metadata.human_readable_size (int64 (total_rvn_size)));
      Event.emit_notice ("Disk space required to install these packages: " &
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
      answer := (10.0 * float_num / float_den) + 0.5;
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

end Raven.Database.Fetch;
