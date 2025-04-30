--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Unchecked_Conversion;
with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Real_Time;
with Ada.Direct_IO;
with Ada.Directories;
with Ada.Characters.Latin_1;
with Raven.Strings;
with Archive.Unix;

package body curl_callbacks is

   package RT  renames Ada.Real_Time;
   package STM renames Ada.Streams;
   package FIX renames Ada.Strings.Fixed;
   package LAT renames Ada.Characters.Latin_1;
   package STR renames Raven.Strings;

   ------------------
   --  write_file  --
   ------------------
   function write_file (ptr      : access IC.unsigned_char;
                        size     : IC.size_t;
                        nmemb    : IC.size_t;
                        userdata : System.Address) return IC.size_t
   is
      backfour : constant String (1 .. 4) := LAT.BS & LAT.BS & LAT.BS & LAT.BS;
      bytes_passed : constant Transfer_Size := Transfer_Size (nmemb);
      zdata : curldata;
      for zdata'Address use userdata;
      pragma Import (Ada, zdata);
   begin
      if bytes_passed = 0 then
         return IC.size_t (0);
      end if;

      declare
         type cdatatype is array (1 .. nmemb) of IC.unsigned_char;
         subtype adatatype is STM.Stream_Element_Array (1 .. STM.Stream_Element_Offset (nmemb));

         function char_to_stream is new Ada.Unchecked_Conversion
           (Source => cdatatype,
            Target => adatatype);

         cdata : cdatatype;
         for cdata'Address use ptr.all'Address;
         adata : adatatype := char_to_stream (cdata);

      begin
         SIO.Write (zdata.file_handle, adata);
      end;

      zdata.progress := zdata.progress + bytes_passed;

      if zdata.display_pc then
         if zdata.file_size = 0 then
            Ada.Text_IO.Put (backfour & "----");
         else
            declare
               k : constant Transfer_Size := zdata.progress * 10005;
               p : Transfer_Size := k / zdata.file_size;
            begin
               if p >= 10000 then
                  Ada.Text_IO.Put (backfour & "100%");
               else
                  declare
                     z4 : constant String := STR.zeropad (Natural (p), 4);
                  begin
                     Ada.Text_IO.Put (backfour & z4 (z4'First .. z4'First + 1) & LAT.Full_Stop &
                                        z4 (z4'First + 2));
                  end;
               end if;
            end;
         end if;
      end if;
      return nmemb;

   end write_file;


   ----------------------
   --  process_header  --
   ----------------------
   function process_header
     (ptr      : access IC.unsigned_char;
      size     : IC.size_t;
      nmemb    : IC.size_t;
      userdata : System.Address) return IC.size_t
   is
      zdata : curldata;
      for zdata'Address use userdata;
      pragma Import (Ada, zdata);
   begin
      declare
         subtype cdatatype is String (1 .. Natural (nmemb));

         cdata : cdatatype;
         for cdata'Address use ptr.all'Address;
         delim : Natural;
         equalsign : Natural;
      begin
         --  Ada.Text_IO.Put_Line (cdata (cdata'First .. cdata'Last - 2));
         delim := FIX.index (cdata, ":");
         if delim > 0 then
            if cdata (cdata'First .. delim) = "Cache-Control:" then
               equalsign := FIX.index (cdata, "=", delim);
               if equalsign > 0 then
                  if cdata (cdata'First .. equalsign) = "Cache-Control: max-age=" then
                     begin
                        zdata.max_age := Natural'Value (cdata (equalsign + 1 .. cdata'Last - 2));
                     exception
                        when others => null;
                     end;
                  end if;
               end if;
            elsif cdata (cdata'First .. delim) = "ETag:" then
               write_etag_file (ASU.To_String (zdata.etag_file), extract_etag (cdata));
            end if;
         end if;
      end;

      return nmemb;
   end process_header;


   --------------------
   --  extract_etag  --
   --------------------
   function extract_etag (raw_etag : String) return String
   is
      myquote : constant String (1 .. 1) := (1 => '"');
      first_quote : Natural;
      second_quote : Natural;
   begin
      first_quote := FIX.index (raw_etag, myquote);
      if first_quote > 0 then
         second_quote := FIX.index (raw_etag, myquote, first_quote + 1);
         if second_quote > 0 then
            return raw_etag (first_quote + 1 .. second_quote - 1);
         end if;
      end if;
      return "";
   end extract_etag;


   -----------------------
   --  write_etag_file  --
   -----------------------
   procedure write_etag_file (filename : String; etag : String)
   is
      File_Size : constant Natural := etag'Length;

      subtype File_String    is String (1 .. File_Size);
      package File_String_IO is new Ada.Direct_IO (File_String);

      file_handle : File_String_IO.File_Type;
   begin
      File_String_IO.Create (File => file_handle,
                             Mode => File_String_IO.Out_File,
                             Name => filename);
      File_String_IO.Write  (File => file_handle,
                             Item => etag);
      File_String_IO.Close  (file_handle);
   exception
      when Storage_Error =>
         if File_String_IO.Is_Open (file_handle) then
            File_String_IO.Close (file_handle);
         end if;
      when others =>
         if File_String_IO.Is_Open (file_handle) then
            File_String_IO.Close (file_handle);
         end if;
   end write_etag_file;


   -----------------------
   --  found_etag_file  --
   -----------------------
   function found_etag_file (filename : String) return Boolean
   is
      filetc : Archive.Unix.Time_Characteristics;
   begin
      filetc := Archive.Unix.get_time_characteristics (filename);
      case filetc.ftype is
         when Archive.regular =>
            return True;
         when others =>
            null;
      end case;
      return False;
   end found_etag_file;


   -----------------------------
   --  target_file_cached #1  --
   -----------------------------
   function target_file_cached (target_file : String; etag_file : String) return Boolean
   is
      target : Archive.Unix.Time_Characteristics;
      filetc : Archive.Unix.Time_Characteristics;
   begin
      target := Archive.Unix.get_time_characteristics (target_file);
      case target.ftype is
         when Archive.regular =>
            filetc := Archive.Unix.get_time_characteristics (etag_file);
            case filetc.ftype is
               when Archive.regular =>
                  return not Archive.Unix.tag_expired (filetc.mtime);
               when others =>
                  null;
            end case;
         when others =>
            null;
      end case;
      return False;
   end target_file_cached;


   -----------------------------
   --  target_file_cached #2  --
   -----------------------------
   function target_file_cached (target_file : String) return Boolean
   is
      target : Archive.Unix.Time_Characteristics;
   begin
      target := Archive.Unix.get_time_characteristics (target_file);
      case target.ftype is
         when Archive.regular =>
            return not Archive.Unix.tag_expired (target.mtime);
         when others =>
            null;
      end case;
      return False;
   end target_file_cached;


   ----------------------
   --  file_to_string  --
   ----------------------
   function file_to_string (filename : String) return String
   is
      filesize : constant Natural := Natural (Ada.Directories.Size (filename));
   begin
      declare
         subtype File_String    is String (1 .. filesize);
         package File_String_IO is new Ada.Direct_IO (File_String);
         File     : File_String_IO.File_Type;
         Contents : File_String;
      begin
         File_String_IO.Open (File => File,
                              Mode => File_String_IO.In_File,
                              Name => filename);
         File_String_IO.Read (File => File,
                              Item => Contents);
         File_String_IO.Close (File);
         return Contents;
      exception
         when others =>
            if File_String_IO.Is_Open (File) then
               File_String_IO.Close (File);
            end if;
            return "";
      end;
   end file_to_string;


   ---------------------------
   --  set_expiration_time  --
   ---------------------------
   function set_expiration_time (path : String; max_age : Natural) return Boolean
   is
      use type Archive.filetime;

      filetc : Archive.Unix.Time_Characteristics;
      rc     : Archive.Unix.metadata_rc;
   begin
      filetc := Archive.Unix.get_time_characteristics (path);
      case filetc.ftype is
         when Archive.regular =>
            rc := Archive.Unix.adjust_metadata
              (path         => path,
               reset_owngrp => False,
               reset_perms  => False,
               reset_mtime  => True,
               type_of_file => filetc.ftype,
               new_uid      => 0,
               new_gid      => 0,
               new_perms    => 0,
               new_m_secs   => filetc.mtime + Archive.filetime (max_age),
               new_m_nano   => filetc.mnsec);
            case rc is
               when 0 => return True;
               when others => null;
            end case;
         when others => null;
      end case;
      return False;
   end set_expiration_time;


   ----------------------------------
   --  randomized_download_target  --
   ----------------------------------
   function randomized_download_target (true_path : String) return String
   is
      right_now : constant RT.Time := RT.Clock;
      seconds   : RT.Seconds_Count;
      nanospan  : RT.Time_Span;
      nduration : Duration;
   begin
      RT.Split (right_now, seconds, nanospan);
      nduration := RT.To_Duration (nanospan);
      declare
         durstr : constant String := nduration'Img;
      begin
         --  durstr in format " 0.xxxxxxxxx" (leading space)
         return true_path & durstr (durstr'First + 2 .. durstr'Last);
      end;
   end randomized_download_target;


   -----------------------------
   --  rename_temporary_file  --
   -----------------------------
   procedure rename_temporary_file (true_path, temporary_path : String) is
   begin
      if Ada.Directories.Exists (true_path) then
         begin
            Ada.Directories.Delete_File (true_path);
         exception
            when others => return;
         end;
      end if;
      if Ada.Directories.Exists (temporary_path) then
         begin
            Ada.Directories.Rename (temporary_path, true_path);
         exception
            when others => return;
         end;
      end if;
   end rename_temporary_file;


   -----------------------------
   --  remove_temporary_file  --
   -----------------------------
   procedure remove_temporary_file (temporary_path : String) is
   begin
      if Ada.Directories.Exists (temporary_path) then
         begin
            Ada.Directories.Delete_File (temporary_path);
         exception
            when others => return;
         end;
      end if;
   end remove_temporary_file;


   ------------------
   --  first_line  --
   ------------------
   function first_line (contents : String) return String
   is
      LF : constant String (1 .. 1) := (1 => Character'Val (10));
      next_lf : Natural;
   begin
      next_lf := FIX.Index (contents, LF);
      if next_lf > 0 then
         return contents (contents'First .. next_lf - 1);
      end if;
      return contents;
   end first_line;

end curl_callbacks;
