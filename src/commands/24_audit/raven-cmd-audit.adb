--  SPDX-License-Identifier: ISC
--  Reference: /License.txt


with Ada.Text_IO;
with Ada.Environment_Variables;
with Archive.Unix;
with Raven.Event;
with Raven.Fetch;
with Raven.Strings;
with Raven.Cmd.Unset;
with Raven.Database.Lock;
with Raven.Database.Annotate;
with Raven.Database.Operations;
with ThickUCL.Files;
with ThickUCL.Emitter;
with Ucl;

use Raven.Strings;

package body Raven.Cmd.Audit is

   package ENV renames Ada.Environment_Variables;
   package TIO renames Ada.Text_IO;
   package RCU renames Raven.Cmd.Unset;
   package LOK renames Raven.Database.Lock;
   package ANN renames Raven.Database.Annotate;
   package OPS renames Raven.Database.Operations;


   -----------------------------
   --  execute_audit_command  --
   -----------------------------
   function execute_audit_command (comline : Cldata) return Boolean
   is
      --  Undocumented: environ variable TESTAUDIT which contains path to text file of ucl/json
      --                object to send to vuln server.  format:
      --                | notes {
      --                |    cpe = "<cpestring>"
      --                |    nvv = "<namebase 1>:<variant 1>:<version 1>"
      --                |    nvv = "<namebase 2>:<variant 2>:<version 2>"  # only 1 nvv required
      --                | }
      --                | notes { ... }
      --  json format equivalent:
      --                { "notes": [ {"cpe": "cpe string #1", "nvv": [ "nvv#1" (,"nvv#2") ]}, .. ]}
      --  Undocumented: environ variable CVEFIXES which contains path to text file that lists
      --                one CVE id per line.  These represents future annotations for patched CVEs

      ev1 : constant String := "TESTAUDIT";
   begin
      if ENV.Exists (ev1) then
         return external_test_input (comline, ENV.Value (ev1));
      end if;
      return internal_input (comline);
   end execute_audit_command;


   ---------------------------
   --  external_test_input  --
   ---------------------------
   function external_test_input (comline : Cldata; testfile : String) return Boolean
   is
      features  : Archive.Unix.File_Characteristics;
      test_tree : ThickUCL.UclTree;
   begin
      features := Archive.Unix.get_charactistics (testfile);
      case features.ftype is
         when Archive.regular => null;
         when others =>
            TIO.Put_Line ("TESTAUDIT is not set to the path of a regular file.");
            TIO.Put_Line ("Audit test failed.");
            return False;
      end case;
      begin
         ThickUCL.Files.parse_ucl_file (test_tree, testfile, "");
      exception
         when ThickUCL.Files.ucl_file_unparseable =>
            TIO.Put_Line ("FATAL: Failed to parse " & testfile);
            return False;
      end;

      declare
         json_contents : constant String := ThickUCL.Emitter.emit_compact_ucl (test_tree, True);
         response      : ThickUCL.UclTree;
         patchset      : Pkgtypes.Text_List.Vector;
         cpe_entries   : set_cpe_entries.Vector;
      begin
         Event.emit_debug (high_level, "test input: " & json_contents);
         if contact_vulnerability_server (comline.cmd_audit.refresh, json_contents, response) then
            set_patched_cves (patchset);
            if not assemble_data (response, patchset, cpe_entries) then
               if not comline.common_options.quiet then
                  TIO.Put_Line
                    ("Contact Ravenports developers: Vulnerability server rejected the input.");
                  TIO.Put_Line ("Provide the " & cached_vinfo & " file with the report.");
               end if;
               return False;
            end if;
            display_report (comline, cpe_entries);
            return True;
         end if;
      end;
      return False;
   end external_test_input;


   ----------------------
   --  internal_input  --
   ----------------------
   function internal_input (comline : Cldata) return Boolean
   is
      input_tree  : ThickUCL.UclTree;
      notes       : ANN.base_note_set.Vector;
      patchset    : Pkgtypes.Text_List.Vector;
      rdb         : Database.RDB_Connection;
      active_lock : constant LOK.lock_type := LOK.lock_readonly;
      open_cpe    : Boolean := False;
      last_cpe    : Text;

      procedure sift (Position : ANN.base_note_set.Cursor)
      is
         note : ANN.base_note renames ANN.base_note_set.Element (Position);
         nvv  : constant String := USS (note.namebase) & ":" & USS (note.variant) & ":" &
                                   USS (note.version);
      begin
         if equivalent (note.note_key, "cpe") then
            if open_cpe then
               if equivalent (note.note_val, last_cpe) then
                  input_tree.insert ("", nvv);
               else
                  input_tree.close_array;   --  nvv
                  input_tree.close_object;  --  note object
                  open_cpe := False;
               end if;
            end if;
            if not open_cpe then
               input_tree.start_object ("");
               input_tree.insert ("cpe", USS (note.note_val));
               input_tree.start_array ("nvv");
               input_tree.insert ("", nvv);
            end if;
         elsif equivalent (note.note_val, "vulnerability_patched") then
            patchset.Append (note.note_key);
         end if;
      end sift;
   begin
      patchset.Clear;
      case OPS.rdb_open_localdb (rdb, Database.installed_packages) is
         when RESULT_OK => null;
         when others => return False;
      end case;

      if not LOK.obtain_lock (rdb, active_lock) then
         Event.emit_error (LOK.no_read_lock);
         OPS.rdb_close (rdb);
         return False;
      end if;

      ANN.acquire_base_annotations (rdb, notes);

      if not LOK.release_lock (rdb, active_lock) then
         Event.emit_error (LOK.no_read_unlock);
         OPS.rdb_close (rdb);
         return False;
      end if;

      OPS.rdb_close (rdb);
      input_tree.start_array ("notes");
      notes.Iterate (sift'Access);
      if open_cpe then
         input_tree.close_array;   --  nvv
         input_tree.close_object;  --  note object
         open_cpe := False;
      end if;
      input_tree.close_array;

      declare
         json_contents : constant String := ThickUCL.Emitter.emit_compact_ucl (input_tree, True);
         response      : ThickUCL.UclTree;
         cpe_entries   : set_cpe_entries.Vector;
      begin
         Event.emit_debug (high_level, "test input: " & json_contents);
         if contact_vulnerability_server (comline.cmd_audit.refresh, json_contents, response) then
            if not assemble_data (response, patchset, cpe_entries) then
               if not comline.common_options.quiet then
                  TIO.Put_Line
                    ("Contact Ravenports developers: Vulnerability server rejected the input.");
                  TIO.Put_Line ("Provide the " & cached_vinfo & " file with the report.");
               end if;
               return False;
            end if;
            display_report (comline, cpe_entries);
            return True;
         end if;
      end;
      return False;

   end internal_input;


   ------------------------------------
   --  contact_vulnerability_server  --
   ------------------------------------
   function contact_vulnerability_server (refresh       : Boolean;
                                          json_input    : String;
                                          response_tree : in out ThickUCL.UclTree) return Boolean
   is
      cache_dir : constant String := Context.reveal_cache_directory & "/version";
      etag_file : constant String := cache_dir & "/vulninfo.etag";
      vuln_url  : constant String := RCU.config_setting (RCU.CFG.vuln_server);
   begin
      if refresh then
         Archive.Unix.delete_file_if_it_exists (cached_vinfo);
      end if;
      case Fetch.download_post_response
        (remote_file_url => vuln_url,
         etag_file       => etag_file,
         downloaded_file => cached_vinfo,
         post_body       => json_input,
         post_body_type  => "application/json")
      is
         when Fetch.cache_valid | Fetch.file_downloaded =>
            begin
               ThickUCL.Files.parse_ucl_file
                 (tree    => response_tree,
                  path    => cached_vinfo,
                  nvpairs => "");
               return True;
            exception
               when ThickUCL.Files.ucl_file_unparseable =>
                  TIO.Put_Line ("FATAL: Failed to parse " & cached_vinfo);
            end;
         when Fetch.retrieval_failed =>
            TIO.Put_Line ("FATAL: Failed to get response from " & vuln_url);
      end case;
      return False;
   end contact_vulnerability_server;


   ------------------------
   --  set_patched_cves  --
   ------------------------
   procedure set_patched_cves (patchset : in out Pkgtypes.Text_List.Vector)
   is
      ev2 : constant String := "CVEFIXES";
      features : Archive.Unix.File_Characteristics;
      handle   : TIO.File_Type;
   begin
      patchset.Clear;
      if ENV.Exists (ev2) then
         features := Archive.Unix.get_charactistics (ENV.Value (ev2));
         case features.ftype is
            when Archive.regular =>
               begin
                  TIO.Open (handle, TIO.In_File, ENV.Value (ev2));
                  while not TIO.End_Of_File (handle) loop
                     patchset.Append (SUS (TIO.Get_Line (handle)));
                  end loop;
                  TIO.Close (handle);
               exception
                  when others =>
                     if TIO.Is_Open (handle) then
                        TIO.Close (handle);
                     end if;
               end;
            when others =>
               TIO.Put_Line (ev2 & " is not set to the path of a regular file.");
               TIO.Put_Line ("Patched CVEs not set.");
         end case;
      else
         --  TBW: query rvn_pkgdb for annotations
         null;
      end if;
   end set_patched_cves;


   ---------------------
   --  assemble_data  --
   ---------------------
   function assemble_data
     (response_tree : ThickUCL.UclTree;
      patchset      : Pkgtypes.Text_List.Vector;
      cpe_entries   : in out set_cpe_entries.Vector) return Boolean
   is
      key_success : constant String := "success";
      key_records : constant String := "records";
      res_success : Boolean := False;
      keys        : ThickUCL.jar_string.Vector;

      function get_cve_string (ondx : ThickUCL.object_index; name, default : String) return String
      is
      begin
         declare
            value : constant String := response_tree.get_object_value (ondx, name);
         begin
            return value;
         end;
      exception
         when ThickUCL.ucl_key_not_found =>
            return default;
         when ThickUCL.ucl_type_mismatch =>
            return "TYPEFAIL:" & name;
      end get_cve_string;

      function get_cve_int (ondx : ThickUCL.object_index; name : String; default : Integer)
                            return Integer
      is
      begin
         declare
            value : constant Ucl.ucl_integer := response_tree.get_object_value (ondx, name);
         begin
            return Integer (value);
         end;
      exception
         when ThickUCL.ucl_key_not_found | ThickUCL.ucl_type_mismatch =>
            return default;
      end get_cve_int;

      function get_csvv_version (version : Integer) return String is
      begin
         case version is
            when 20 => return "v2.0";
            when 30 => return "v3.0";
            when 31 => return "v3.1";
            when 40 => return "v4.0";
            when others => return "UNKNOWN " & int2str (version);
         end case;
      end get_csvv_version;

      function get_threat_level (base_score : Integer) return String is
      begin
         case base_score is
            when  0        => return "None";
            when  1 ..  39 => return "Low";
            when 40 ..  69 => return "Medium";
            when 70 ..  89 => return "High";
            when 90 .. 100 => return "Critical";
            when others    => return "ERROR:" & int2str (base_score);
         end case;
      end get_threat_level;

      procedure process_cpe_result (position : ThickUCL.jar_string.Cursor)
      is
         key       : constant String := USS (ThickUCL.jar_string.Element (position).payload);
         rec       : cpe_entry;
         secure    : Boolean := True;
         baseobj   : ThickUCL.object_index;
         dtype     : ThickUCL.Leaf_type;
         vndx      : ThickUCL.array_index;
         narr      : Natural;
         index_nvv : constant String := "nvv";
         index_cve : constant String := "cve";
      begin
         if key = key_success or else key = key_records then
            return;
         end if;

         Event.emit_debug (high_level, "processing " & key & " string");
         baseobj := response_tree.get_index_of_base_ucl_object (key);
         dtype := response_tree.get_object_data_type (baseobj, index_nvv);
         case dtype is
            when ThickUCL.data_array =>
               vndx := response_tree.get_object_array (baseobj, index_nvv);
               narr := response_tree.get_number_of_array_elements (vndx);
               for x in 1 .. narr loop
                  case response_tree.get_array_element_type (vndx, x - 1) is
                     when ThickUCL.data_string =>
                        declare
                           value : constant String :=
                             response_tree.get_array_element_value (vndx, x - 1);
                           num_fields : constant Natural := count_char (value, ':') + 1;
                           nrec : nvv_rec;
                        begin
                           if num_fields = 3 then
                              nrec.namebase := SUS (specific_field (value, 1, ":"));
                              nrec.variant  := SUS (specific_field (value, 2, ":"));
                              nrec.version  := SUS (specific_field (value, 3, ":"));
                              rec.installed_packages.Append (nrec);
                           end if;
                        end;
                     when others => null;
                  end case;
               end loop;
            when others => null;
         end case;

         dtype := response_tree.get_object_data_type (baseobj, index_cve);
         case dtype is
            when ThickUCL.data_array =>
               vndx := response_tree.get_object_array (baseobj, index_cve);
               narr := response_tree.get_number_of_array_elements (vndx);
               for x in 1 .. narr loop
                  case response_tree.get_array_element_type (vndx, x - 1) is
                     when ThickUCL.data_object =>
                        declare
                           crec : cve_rec;
                           ondx : ThickUCL.object_index :=
                             response_tree.get_array_element_object (vndx, x - 1);
                        begin
                           declare
                              f01 : constant String :=
                                get_cve_string (ondx, "cve_id", "CVE-1970-XXXX");
                              f02 : constant String :=
                                get_cve_string (ondx, "published", "1970-01-01 00:00:00");
                              f03 : constant String :=
                                get_cve_string (ondx, "modified", "1970-01-01 00:00:00");
                              f04 : constant String :=
                                get_cve_string (ondx, "description", "Description was missing.");
                              f09 : constant String :=
                                get_cve_string (ondx, "csvv_vector", "VECTOR_MIA");
                              f05 : constant Integer := get_cve_int (ondx, "csvv_version", 0);
                              f06 : constant Integer := get_cve_int (ondx, "csvv_basescore", 0);
                              f07 : constant Integer := get_cve_int (ondx, "csvv_exploit", 0);
                              f08 : constant Integer := get_cve_int (ondx, "csvv_impact", 0);
                           begin
                              crec.cve_id         := SUS (f01);
                              crec.published      := SUS (f02);
                              crec.modified       := SUS (f03);
                              crec.description    := SUS (f04);
                              crec.cvss_vector    := SUS (f09);
                              crec.cvss_version   := f05;
                              crec.base_score     := f06;
                              crec.exploitability := f07;
                              crec.impact         := f08;
                              crec.threat_level   := SUS (get_threat_level (F06));
                              crec.patched        := patchset.Contains (crec.cve_id);
                              rec.vulnerabilities.Append (crec);

                              if not crec.patched then
                                 secure := False;
                              end if;
                           end;
                        end;
                     when others => null;
                  end case;
               end loop;
            when others => null;
         end case;

         rec.secure  := secure;
         rec.vendor  := SUS (specific_field (key, 4, ":"));
         rec.product := SUS (specific_field (key, 5, ":"));
         cpe_entries.Append (rec);
      end process_cpe_result;
   begin
      if response_tree.boolean_field_exists (key_success) then
         res_success := response_tree.get_base_value (key_success);
         if not res_success then
            return False;
         end if;
      end if;

      if not response_tree.integer_field_exists (key_records) then
         return False;
      end if;

      declare
         res_records : Ucl.ucl_integer := 0;
         use type Ucl.ucl_integer;
      begin
         res_records := response_tree.get_base_value (key_records);
         if res_records > 0 then
            response_tree.get_base_object_keys (keys);
            keys.Iterate (process_cpe_result'Access);
         end if;
      end;

      return True;
   end assemble_data;


   ----------------------------
   --  expand_vector_string  --
   ----------------------------
   function expand_vector_string (cvss_version : Integer; vstring: String) return vector_breakdown
   is
      res : vector_breakdown;
      que : constant String := "Unrecognized:";

      function nalp (code : Character) return String is
      begin
         case code is
            when 'N' | 'n' => return "Network";
            when 'A' | 'a' => return "Adjacent";
            when 'L' | 'l' => return "Local";
            when 'P' | 'p' => return "Physical";
            when others    => return que & code;
         end case;
      end nalp;

      function level (code : Character) return String is
      begin
         case code is
            when 'N' | 'n' => return "None";
            when 'L' | 'l' => return "Low";
            when 'M' | 'm' => return "Medium";
            when 'H' | 'h' => return "High";
            when others    => return que & code;
         end case;
      end level;

      function npc (code : Character) return String is
      begin
         case code is
            when 'N' | 'n' => return "None";
            when 'P' | 'p' => return "Partial";
            when 'C' | 'c' => return "Complete";
            when others    => return que & code;
         end case;
      end npc;

      function userint (code : Character) return String is
      begin
         case code is
            when 'N' | 'n' => return "None";     --  4.0 and 2.0
            when 'P' | 'p' => return "Passive";  --  4.0
            when 'A' | 'a' => return "Active";   --  4.0
            when 'R' | 'r' => return "Required"; --  2.0
            when others    => return que & code;
         end case;
      end userint;

      function scope (code : Character) return String is
      begin
         case code is
            when 'C' | 'c' => return "Changed";
            when 'U' | 'u' => return "Unchanged";
            when others    => return que & code;
         end case;
      end scope;

      function authentication (code : Character) return String is
      begin
         case code is
            when 'N' | 'n' => return "None";
            when 'M' | 'm' => return "Multiple";
            when 'S' | 's' => return "Single";
            when others    => return que & code;
         end case;
      end authentication;

      function attreq (code : Character) return String is
      begin
         case code is
            when 'N' | 'n' => return "None";
            when 'P' | 'p' => return "Present";
            when others    => return que & code;
         end case;
      end attreq;

      function letter (index : Integer) return Character is
      begin
         if index > 0 and then index < vstring'Length then
            return vstring (vstring'First + index);
         end if;
         return '^';
      end letter;
   begin
      case cvss_version is
         when 20 =>
            --              0000000001111111111222222
            --             F  3    8    3   7   1   5
            --  cvss 2.0: "AV:L/AC:M/Au:N/C:N/I:P/A:C"
            res.attack_vector     := SUS (nalp (letter (3)));
            res.attack_complexity := SUS (level (letter (8)));
            res.authentication_20 := SUS (authentication (letter (13)));
            res.confidentiality   := SUS (npc (letter (17)));
            res.integrity         := SUS (npc (letter (21)));
            res.availability      := SUS (npc (letter (25)));
         when 30 | 31 =>
            --              00000000011111111112222222222333333333344444
            --             F           2    7    2    7   1   5   9   3
            --  cvss 3.x: "CVSS:3.1/AV:N/AC:L/PR:H/UI:N/S:U/C:L/I:L/A:N"
            res.attack_vector       := SUS (nalp (letter (12)));
            res.attack_complexity   := SUS (level (letter (17)));
            res.privileges_required := SUS (level (letter (22)));
            res.user_interaction    := SUS (userint (letter (27)));
            res.scope               := SUS (scope (letter (31)));
            res.confidentiality     := SUS (level (letter (35)));
            res.integrity           := SUS (level (letter (39)));
            res.availability        := SUS (level (letter (43)));
         when 40 =>
            --              0000000001111111111222222222233333333334444444444555555555566666
            --             F           2    7    2    7    2    7    2    7    2    7    2
            --  cvss 4.0: "CVSS:4.0/AV:N/AC:L/AT:N/PR:H/UI:N/VC:L/VI:L/VA:N/SC:N/SI:N/SA:N"
            res.attack_vector       := SUS (nalp (letter (12)));
            res.attack_complexity   := SUS (level (letter (17)));
            res.attack_reqments_40  := SUS (attreq (letter (22)));
            res.privileges_required := SUS (level (letter (27)));
            res.user_interaction    := SUS (userint (letter (32)));
            res.vsys_confident_40   := SUS (level (letter (37)));
            res.vsys_integrity_40   := SUS (level (letter (42)));
            res.vsys_avail_40       := SUS (level (letter (47)));
            res.ssys_confident_40   := SUS (level (letter (52)));
            res.ssys_integrity_40   := SUS (level (letter (57)));
            res.ssys_avail_40       := SUS (level (letter (62)));
         when others => null;
      end case;
      return res;
   end expand_vector_string;


   --------------------
   --  make_decimal  --
   --------------------
   function make_decimal (score : Integer) return String
   is
      --  3 columns for 9.9 and below
      --  4 columns for 10.0 (to 99.9, but nothing over 10.0 should be possible)
      raw : constant String := int2str (score);
   begin
      if score < 10 then
         return "0." & raw;
      end if;
      if score >= 100 then
         return raw (raw'First .. raw'Last - 1) & '.' & raw (raw'Last);
      end if;
      return raw (raw'First .. raw'Last - 1) & '.' & raw (raw'Last);
   end make_decimal;


   -----------------------------
   --  display_single_record  --
   -----------------------------
   procedure display_single_record
     (comline       : Cldata;
      cpe_entries   : set_cpe_entries.Vector;
      index         : Natural)
   is
      rec : cpe_entry renames cpe_entries (index);
      first_cve : Boolean := True;

      procedure print_package (position : set_nvv.Cursor)
      is
         nvv : nvv_rec renames set_nvv.Element (position);
         basic : constant String :=
           USS (nvv.namebase) & " (" & USS (nvv.variant) & ") " & USS (nvv.version);
      begin
         if comline.common_options.quiet and
           comline.cmd_audit.filter /= Cmd.none
         then
            TIO.Put_Line (basic);
         else
            if rec.secure then
               TIO.Put_Line (basic & " is secure");
            else
               TIO.Put_Line (basic & " is vulnerable");
            end if;
         end if;
      end print_package;

      procedure print_cve (position : set_cve.Cursor)
      is
         cve : cve_rec renames set_cve.Element (position);
         fixed : string := "  PATCHED  ";
      begin
         if not cve.patched then
            fixed := (others => ' ');
         end if;
         TIO.Put_Line ("    " & USS (cve.cve_id) & fixed & "Score: " &
                         make_decimal (cve.base_score) & " " & USS (cve.threat_level));
      end print_cve;

      procedure print_full_cve (position : set_cve.Cursor)
      is
         cve      : cve_rec renames set_cve.Element (position);

         origin   : constant String := USS (rec.vendor) & ":" & USS (rec.product);
         column   : constant Natural := 28;
         vectors  : constant vector_breakdown :=
                    expand_vector_string (cve.cvss_version, USS (cve.cvss_vector));

         label_av : constant String := "    Attack Vector";
         label_ac : constant String := "    Attack Complexity";
         label_ar : constant String := "    Attack Requirements";
         label_au : constant String := "    Authentication";
         label_co : constant String := "    Confidentiality";
         label_in : constant String := "    Integrity";
         label_ay : constant String := "    Availability";
         label_pr : constant String := "    Privileges Required";
         label_ui : constant String := "    User Interaction";
         label_sc : constant String := "    Scope";

         procedure print (left_side : String; right_side : String) is
         begin
            TIO.Put_Line (pad_right (left_side, column) & right_side);
         end print;

         fixed  : string := " (PATCHED)";
      begin
         if not cve.patched then
            fixed := (others => ' ');
         end if;
         if not first_cve then
            TIO.Put_Line ("");
         end if;
         first_cve := False;
         print (USS(cve.cve_id), origin & fixed);
         print ("    Base score:", make_decimal (cve.base_score) & " " & USS (cve.threat_level));
         print ("    Exploitability Score:", make_decimal (cve.exploitability));
         print ("    Impact Score:", make_decimal (cve.impact));
         print ("    CVSS Version:", make_decimal (cve.cvss_version));
         case cve.cvss_version is
            when 20 =>
               print (label_av, USS (vectors.attack_vector));
               print (label_ac, USS (vectors.attack_complexity));
               print (label_au, USS (vectors.authentication_20));
               print (label_co, USS (vectors.confidentiality));
               print (label_in, USS (vectors.integrity));
               print (label_ay, USS (vectors.availability));
            when 30 | 31 =>
               print (label_av, USS (vectors.attack_vector));
               print (label_ac, USS (vectors.attack_complexity));
               print (label_pr, USS (vectors.privileges_required));
               print (label_ui, USS (vectors.user_interaction));
               print (label_sc, USS (vectors.scope));
               print (label_co, USS (vectors.confidentiality));
               print (label_in, USS (vectors.integrity));
               print (label_ay, USS (vectors.availability));
            when 40 =>
               print (label_av, USS (vectors.attack_vector));
               print (label_ac, USS (vectors.attack_complexity));
               print (label_ar, USS (vectors.attack_reqments_40));
               print (label_pr, USS (vectors.privileges_required));
               print (label_ui, USS (vectors.user_interaction));
               print ("    VS Confidentiality", USS (vectors.vsys_confident_40));
               print ("    VS Integrity", USS (vectors.vsys_integrity_40));
               print ("    VS Availability", USS (vectors.vsys_avail_40));
               print ("    SS Confidentiality", USS (vectors.ssys_confident_40));
               print ("    SS Integrity", USS (vectors.ssys_integrity_40));
               print ("    SS Availability", USS (vectors.ssys_avail_40));
            when others =>
               null;
         end case;
         print ("    NVD Published:", USS (cve.published));
         print ("    NVD Last Modified:", USS (cve.modified));
         print ("    Link", "https://nvd.nist.gov/vuln/detail/" & USS (cve.cve_id));
         print_description (USS (cve.description));
      end print_full_cve;
   begin
      case comline.cmd_audit.filter is
         when vulnerable =>
            if rec.secure then
               return;
            end if;
         when secure =>
            if not rec.secure then
               return;
            end if;
         when none => null;
      end case;

      rec.installed_packages.Iterate (print_package'Access);
      case comline.cmd_audit.level is
         when summary => return;
         when concise =>
            rec.vulnerabilities.Iterate (print_cve'Access);
         when full =>
            rec.vulnerabilities.Iterate (print_full_cve'Access);
      end case;
      TIO.Put_Line ("");

   end display_single_record;


   ----------------------------
   --  encode_single_record  --
   ----------------------------
   procedure encode_single_record
     (tree          : in out ThickUCL.UclTree;
      comline       : Cldata;
      cpe_entries   : set_cpe_entries.Vector;
      index         : Natural)
   is
      rec : cpe_entry renames cpe_entries (index);

      procedure save_package (position : set_nvv.Cursor)
      is
         nvv : nvv_rec renames set_nvv.Element (position);
      begin
         tree.start_object ("");
         tree.insert ("namebase", USS (nvv.namebase));
         tree.insert ("variant", USS (nvv.variant));
         tree.insert ("version", USS (nvv.version));
         tree.close_object;
      end save_package;

      procedure save_cve (position : set_cve.Cursor)
      is
         cve : cve_rec renames set_cve.Element (position);
         vectors  : constant vector_breakdown :=
           expand_vector_string (cve.cvss_version, USS (cve.cvss_vector));
      begin
         tree.start_object ("");
         tree.insert ("cve_id", USS (cve.cve_id));
         tree.insert ("patched", cve.patched);
         tree.insert ("base_score", make_decimal (cve.base_score));
         tree.insert ("threat_level", USS (cve.threat_level));
         case comline.cmd_audit.level is
            when summary | concise => null;
            when full =>
               tree.insert ("exploit_score", make_decimal (cve.exploitability));
               tree.insert ("impact_score", make_decimal (cve.impact));
               tree.insert ("published", USS (cve.published));
               tree.insert ("modified", USS (cve.modified));
               tree.insert ("link", "https://nvd.nist.gov/vuln/detail/" & USS (cve.cve_id));
               tree.insert ("description", USS (cve.description));
               tree.insert ("cvss_version", make_decimal (cve.cvss_version));
               tree.insert ("attack_vector", USS (vectors.attack_vector));
               tree.insert ("attack_complexity", USS (vectors.attack_complexity));
               case cve.cvss_version is
                  when 20 =>
                     tree.insert ("authentication", USS (vectors.authentication_20));
                     tree.insert ("confidentiality", USS (vectors.confidentiality));
                     tree.insert ("integrity", USS (vectors.integrity));
                     tree.insert ("availability", USS (vectors.availability));
                  when 30 | 31 =>
                     tree.insert ("privileges_req", USS (vectors.privileges_required));
                     tree.insert ("scope", USS (vectors.scope));
                     tree.insert ("confidentiality", USS (vectors.confidentiality));
                     tree.insert ("integrity", USS (vectors.integrity));
                     tree.insert ("availability", USS (vectors.availability));
                  when 40 =>
                     tree.insert ("attack_req", USS (vectors.attack_reqments_40));
                     tree.insert ("privileges_req", USS (vectors.privileges_required));
                     tree.insert ("user_interaction",  USS (vectors.user_interaction));
                     tree.insert ("vs_confidentiality", USS (vectors.vsys_confident_40));
                     tree.insert ("ss_confidentiality", USS (vectors.ssys_confident_40));
                     tree.insert ("vs_integrity", USS (vectors.vsys_integrity_40));
                     tree.insert ("ss_integrity", USS (vectors.ssys_integrity_40));
                     tree.insert ("vs_availability", USS (vectors.vsys_integrity_40));
                     tree.insert ("ss_availability", USS (vectors.ssys_avail_40));
                  when others =>
                     null;
               end case;
         end case;
         tree.close_object;
      end save_cve;
   begin
      case comline.cmd_audit.filter is
         when vulnerable =>
            if rec.secure then
               return;
            end if;
         when secure =>
            if not rec.secure then
               return;
            end if;
         when none => null;
      end case;

      if rec.secure then
         tree.reopen_array ("secure");
      else
         tree.reopen_array ("vulnerable");
      end if;

      tree.start_object ("");
      tree.insert ("vendor", USS (rec.vendor));
      tree.insert ("product", USS (rec.product));

      tree.start_array ("installed");
      rec.installed_packages.Iterate (save_package'Access);
      tree.close_array;

      tree.start_array ("cve");
      rec.vulnerabilities.Iterate (save_cve'Access);
      tree.close_array;

      tree.close_object;
      tree.close_array;
   end encode_single_record;


   ----------------------
   --  display_report  --
   ----------------------
   procedure display_report
     (comline       : Cldata;
      cpe_entries   : set_cpe_entries.Vector)
   is
      total_shown   : Natural := 0;
      total_records : constant Natural := Natural (cpe_entries.Length);
      structured    : ThickUCL.UclTree;
   begin
      for x in 1 .. total_records loop
         case comline.cmd_audit.filter is
            when vulnerable =>
               if not cpe_entries.Element(x - 1).secure then
                  total_shown := total_shown + 1;
               end if;
            when secure =>
               if cpe_entries.Element(x - 1).secure then
                  total_shown := total_shown + 1;
               end if;
            when none =>
               total_shown := total_shown + 1;
         end case;
      end loop;

      if total_shown = 0 then
         if not comline.common_options.quiet then
            TIO.Put_Line ("There is no vulnerability data to display.");
         end if;
         return;
      end if;

      structured.start_array ("secure");
      structured.close_array;
      structured.start_array ("vulnerable");
      structured.close_array;

      for x in 1 .. total_records loop
         case comline.cmd_audit.format is
            when fmt_report =>
               display_single_record (comline, cpe_entries, x - 1);
            when fmt_json | fmt_ucl =>
               encode_single_record (structured, comline, cpe_entries, x - 1);
         end case;
      end loop;

      case comline.cmd_audit.format is
         when fmt_report => null;
         when fmt_json =>
            TIO.Put_Line (ThickUCL.Emitter.emit_compact_ucl (structured, True));
         when fmt_ucl =>
            TIO.Put_Line (ThickUCL.Emitter.emit_ucl (structured));
      end case;

   end display_report;


   -------------------------
   --  print_description  --
   -------------------------
   procedure print_description (description : String)
   is
      maxwidth : constant Natural := raven.Context.reveal_terminal_width - 5;
      final    : constant Integer := description'Last;
      marker   : Integer := description'First;
      index    : Integer;
   begin
      loop
         if final - marker + 1 <= maxwidth then
            TIO.Put_Line (description (marker .. description'Last));
            exit;
         end if;
         index := marker + maxwidth;
         loop
            if description (index) = ' ' then
               exit;
            end if;
            index := index - 1;
            if index = marker then
               --  no breaks found.   (Not normal description, just break string after full width)
               index := marker + maxwidth;
               exit;
            end if;
         end loop;
         TIO.Put_Line (description (marker .. index - 1));
         marker := index + 1;  -- essentially space at "index" is converted to line feed
      end loop;
   end print_description;


end Raven.Cmd.Audit;
