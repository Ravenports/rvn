--  SPDX-License-Identifier: ISC
--  Reference: /License.txt


with Ada.Text_IO;
with Ada.Direct_IO;
with Ada.Environment_Variables;
with Archive.Unix;
with Raven.Fetch;
with Raven.Context;
with Raven.Cmd.Unset;
with ThickUCL.Files;

package body Raven.Cmd.Audit is

   package ENV renames Ada.Environment_Variables;
   package TIO renames Ada.Text_IO;
   package RCU renames Raven.Cmd.Unset;


   -----------------------------
   --  execute_audit_command  --
   -----------------------------
   function execute_audit_command (comline : Cldata) return Boolean
   is
      --  Undocumented: environ variable TESTAUDIT which contains path to text file of json object
      --                to send to vuln server.  format: {"cpe": [ list of CPE strings ]}
      --                environ variable CVEFIXES which contains path to text file that lists
      --                one CVE id per line.  These represents future annotations for patched CVEs

      ev1 : constant String := "TESTAUDIT";
      ev2 : constant String := "CVEFIXES";
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
      features : Archive.Unix.File_Characteristics;
   begin
      features := Archive.Unix.get_charactistics (testfile);
      case features.ftype is
         when Archive.regular => null;
         when others =>
            TIO.Put_Line ("TESTAUDIT is not set to the path of a regular file.");
            TIO.Put_Line ("Audit test failed.");
            return False;
      end case;
      declare
         contents : constant String := file_contents (testfile, Natural (features.size));
         response : ThickUCL.UclTree;
      begin
         if contact_vulnerability_server (comline.cmd_audit.refresh, contents, response) then
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
         post_body       => json_input)
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

end Raven.Cmd.Audit;
