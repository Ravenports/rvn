--  SPDX-License-Identifier: ISC
--  Reference: /License.txt


with Ada.Text_IO;
with Ada.Direct_IO;
with Ada.Environment_Variables;
with Archive.Unix;
with Raven.Fetch;
with Raven.Strings;
with Raven.Context;
with Raven.Cmd.Unset;
with ThickUCL.Files;
with ThickUCL.Emitter;
with Ucl;

use Raven.Strings;

package body Raven.Cmd.Audit is

   package ENV renames Ada.Environment_Variables;
   package TIO renames Ada.Text_IO;
   package RCU renames Raven.Cmd.Unset;


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
      TIO.Put_Line ("To be implemented.");
      return False;
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
         cached_file   : constant String := Context.reveal_cache_directory &
                                            "/version/vulninfo.json";
         response      : ThickUCL.UclTree;
         patchset      : Pkgtypes.Text_List.Vector;
         cpe_entries   : set_cpe_entries.Vector;
      begin
         if contact_vulnerability_server (comline.cmd_audit.refresh, json_contents, response) then
            set_patched_cves (patchset);
            if not assemble_data (response, patchset, cpe_entries) then
               if not comline.common_options.quiet then
                  TIO.Put_Line
                    ("Contact Ravenports developers: Vulnerability server rejected the input.");
                  TIO.Put_Line ("Provide the " & cached_file & " file with the report.");
               end if;
               return False;
            end if;
            TIO.Put_Line ("Successful response, rest TBW");
            return True;
         end if;
      end;
      return False;
   end external_test_input;


   ---------------------
   --  file_contents  --
   ---------------------
   function file_contents (filename : String; filesize : Natural) return String
   is
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
   end file_contents;


   ------------------------------------
   --  contact_vulnerability_server  --
   ------------------------------------
   function contact_vulnerability_server (refresh       : Boolean;
                                          json_input    : String;
                                          response_tree : in out ThickUCL.UclTree) return Boolean
   is
      cache_dir : constant String := Context.reveal_cache_directory & "/version";
      etag_file : constant String := cache_dir & "/vulninfo.etag";
      json_file : constant String := cache_dir & "/vulninfo.json";
      vuln_url  : constant String := RCU.config_setting (RCU.CFG.vuln_server);
   begin
      if refresh then
         Archive.Unix.delete_file_if_it_exists (json_file);
      end if;
      case Fetch.download_post_response
        (remote_file_url => vuln_url,
         etag_file       => etag_file,
         downloaded_file => json_file,
         post_body       => json_input,
         post_body_type  => "application/json")
      is
         when Fetch.cache_valid | Fetch.file_downloaded =>
            begin
               ThickUCL.Files.parse_ucl_file
                 (tree    => response_tree,
                  path    => json_file,
                  nvpairs => "");
               return True;
            exception
               when ThickUCL.Files.ucl_file_unparseable =>
                  TIO.Put_Line ("FATAL: Failed to parse " & json_file);
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
                              crec.base_score     := Float (f06) / 10.0;
                              crec.exploitability := Float (f07) / 10.0;
                              crec.impact         := Float (f08) / 10.0;
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


   ----------------------
   --  display_report  --
   ----------------------
   function display_report
     (comline       : Cldata;
      cached_file   : String;
      response_tree : ThickUCL.UclTree;
      patchset      : Pkgtypes.Text_List.Vector) return Boolean
   is
      key_success : constant String := "success";
      key_records : constant String := "records";
      res_success : Boolean := False;
      res_records : Ucl.ucl_integer := 0;
      total_shown : Integer := 0;
      cpe_entries : set_cpe_entries.Vector;
      structured  : ThickUCL.UclTree;
      keys        : ThickUCL.jar_string.Vector;

      use type Ucl.ucl_integer;
   begin
      if response_tree.boolean_field_exists (key_success) then
         res_success := response_tree.get_base_value (key_success);
         if not res_success then
            if not comline.common_options.quiet then
               TIO.Put_Line
                 ("Contact Ravenports developers: Vulnerability server rejected the input");
               TIO.Put_Line ("Provide the " & cached_file & " file with the report.");
            end if;
            return False;
         end if;
      end if;
      if response_tree.integer_field_exists (key_records) then
         res_records := response_tree.get_base_value (key_records);
      end if;
      if res_records > 0 then
         response_tree.get_base_object_keys (keys);
         --  keys.Iterate (process_cpe_result'Access);
      end if;

      if total_shown = 0 then
         if not comline.common_options.quiet then
            TIO.Put_Line ("There is no vulnerability data to display.");
         end if;
      else
         case comline.cmd_audit.format is
            when fmt_report => null;
            when fmt_json =>
               TIO.Put_Line (ThickUCL.Emitter.emit_compact_ucl (structured, True));
            when fmt_ucl =>
               TIO.Put_Line (ThickUCL.Emitter.emit_ucl (structured));
         end case;
      end if;

      return True;
   end display_report;

end Raven.Cmd.Audit;
