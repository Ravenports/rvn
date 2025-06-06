--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Directories;
with Ada.Characters.Latin_1;
with Ada.Environment_Variables;
with Raven.Event;
with Raven.Strings;
with Raven.Context;
with Raven.Cmd.Unset;
with curl_header;
with curl_callbacks;
with Archive.Unix;

package body Raven.Fetch is

   package CAL renames curl_callbacks;
   package RCU renames Raven.Cmd.Unset;
   package DIR renames Ada.Directories;
   package LAT renames Ada.Characters.Latin_1;
   package ENV renames Ada.Environment_Variables;

   ---------------------
   --  download_file  --
   ---------------------
   function download_file
     (remote_file_url : String;
      etag_file       : String;
      downloaded_file : String;
      remote_repo     : Boolean := False;
      remote_protocol : IP_support := no_restriction;
      remote_prv_key  : String := "";
      remote_pub_key  : String := "";
      show_progress   : Boolean := False;
      download_size   : Pkgtypes.Package_Size := 0) return fetch_result
   is
      temporary_file  : constant String := CAL.randomized_download_target (downloaded_file);

      data : CAL.curldata;
      curlobj : curl_header.CURLX;
      response_code : Long_Integer;
      header_list : curl_header.access_curl_slist := null;
      successful_execution : Boolean;
   begin
      if using_file_protocol (remote_file_url) then
         return copy_local_file (remote_file_url, downloaded_file);
      end if;

      if CAL.target_file_cached (downloaded_file, etag_file) then
         Event.emit_debug (high_level, "Latest " & downloaded_file & " is already cached.");
         return cache_valid;
      end if;

      data.progress := 0;
      data.file_size := CAL.Transfer_Size (download_size);
      data.display_pc := show_progress;
      data.etag_file := CAL.ASU.To_Unbounded_String (etag_file);
      begin
         CAL.SIO.Create (data.file_handle, CAL.SIO.Out_File, temporary_file);
      exception
         when others =>
            Event.emit_debug (high_level, "Failed to create " & temporary_file & " for writing");
            return retrieval_failed;
      end;

      curlobj := curl_header.curl_easy_init;
      data.curlobj := curlobj;

      curl_header.set_curl_option (curlobj, curl_header.CURLOPT_NOPROGRESS, True);
      curl_header.set_curl_option (curlobj, curl_header.CURLOPT_TRANSFER_ENCODING, True);
      curl_header.set_curl_option (curlobj, curl_header.CURLOPT_URL, remote_file_url);
      curl_header.set_curl_option (curlobj, curl_header.CURLOPT_WRITEDATA, data'Address);
      curl_header.set_curl_option (curlobj, curl_header.CURLOPT_HEADERDATA, data'Address);
      curl_header.set_write_callback (curlobj, CAL.write_file'Access);
      curl_header.set_header_callback (curlobj, CAL.process_header'Access);

      declare
         verbose : Boolean;
         agent   : constant String := RCU.config_setting_as_string (RCU.CFG.user_agent);
         no_verify_peer : constant Boolean := ENV.Exists ("SSL_NO_VERIFY_PEER");
         no_verify_host : constant Boolean := ENV.Exists ("SSL_NO_VERIFY_HOSTNAME");
         http_proxy     : constant String := ENV.Value ("HTTP_PROXY", "");
         ssl_key        : constant String := ENV.Value ("SSL_CLIENT_KEY_FILE", "");
         ssl_cert       : constant String := ENV.Value ("SSL_CLIENT_CERT_FILE", "");
         cert_file      : constant String := ENV.Value ("SSL_CA_CERT_FILE", "");
         netrc_file     : constant String := ENV.Value ("NETRC", "");
         set_protocol   : IP_support;
      begin
         if agent /= "" then
            curl_header.set_curl_option (curlobj, curl_header.CURLOPT_USERAGENT, agent);
         end if;
         case Raven.Context.reveal_debug_level is
            when silent | high_level => verbose := False;
            when others => verbose := True;
         end case;
         if remote_repo then
            set_protocol := remote_protocol;
         else
            set_protocol := Raven.Context.reveal_protocol_restriction;
         end if;
         case set_protocol is
            when no_restriction => null;
            when IPv4_only => curl_header.set_curl_option (curlobj,
                                                           curl_header.CURLOPT_IPRESOLVE,
                                                           curl_header.CURL_IPRESOLVE_V4);
            when IPv6_only => curl_header.set_curl_option (curlobj,
                                                           curl_header.CURLOPT_IPRESOLVE,
                                                           curl_header.CURL_IPRESOLVE_V6);
         end case;
         curl_header.set_curl_option (curlobj, curl_header.CURLOPT_VERBOSE, verbose);
         if remote_prv_key /= "" then
            curl_header.set_curl_option
              (curlobj, curl_header.CURLOPT_SSH_PRIVATE_KEYFILE, remote_prv_key);
         end if;
         if remote_pub_key /= "" then
            curl_header.set_curl_option
              (curlobj, curl_header.CURLOPT_SSH_PUBLIC_KEYFILE, remote_pub_key);
         end if;
         if no_verify_peer then
            curl_header.set_curl_option (curlobj, curl_header.CURLOPT_SSL_VERIFYPEER, False);
         end if;
         if no_verify_host then
            curl_header.set_curl_option (curlobj, curl_header.CURLOPT_SSL_VERIFYHOST, 0);
         end if;
         if http_proxy /= "" then
            curl_header.set_curl_option (curlobj, curl_header.CURLOPT_PROXY, http_proxy);
         end if;
         if ssl_key /= "" then
            curl_header.set_curl_option (curlobj, curl_header.CURLOPT_SSLKEY, ssl_key);
         end if;
         if ssl_cert /= "" then
            curl_header.set_curl_option (curlobj, curl_header.CURLOPT_SSLCERT, ssl_cert);
         end if;
         if cert_file /= "" then
            curl_header.set_curl_option (curlobj, curl_header.CURLOPT_CAINFO, cert_file);
         end if;
         if netrc_file /= "" then
            curl_header.set_curl_option (curlobj, curl_header.CURLOPT_NETRC_FILE, netrc_file);
         end if;
      end;

      if CAL.found_etag_file (etag_file) and Archive.Unix.file_exists (downloaded_file) then
         declare
            set_etag : constant String := "If-None-Match: " &
              LAT.Quotation & CAL.file_to_string (etag_file) & LAT.Quotation;
         begin
            curl_header.build_header (header_list, set_etag);
            curl_header.set_curl_option (curlobj, curl_header.CURLOPT_HTTPHEADER, header_list);
         end;
      end if;

      successful_execution := curl_header.execute_curl (curlobj);
      CAL.SIO.Close (data.file_handle);
      curl_header.curl_slist_free_all (header_list);

      if CAL.found_etag_file (etag_file) then
         --  Not all URLs produce etag files
         if not CAL.set_expiration_time (etag_file, data.max_age) then
            Event.emit_debug (high_level, "Failed to set mtime of " & etag_file & " in the future");
            return retrieval_failed;
         end if;
      end if;

      response_code := curl_header.get_info_value_long (curlobj,
                                                        curl_header.CURLINFO_RESPONSE_CODE);
      if not Strings.leads (remote_file_url, "http") then
         --  SCP protocol
         if successful_execution then
            CAL.rename_temporary_file (downloaded_file, temporary_file);
            return file_downloaded;
         else
            CAL.remove_temporary_file (temporary_file);
            return retrieval_failed;
         end if;
      end if;

      --  Below pertains to HTTP/HTTPS protocols
      if response_code = 200 then
         CAL.rename_temporary_file (downloaded_file, temporary_file);
      else
         CAL.remove_temporary_file (temporary_file);
      end if;
      if Archive.Unix.file_exists (downloaded_file) then
         if response_code = 200 then
            Event.emit_debug (high_level, "Received file " & downloaded_file);
            return file_downloaded;
         end if;
         if response_code = 304 then
            Event.emit_debug (high_level, "Response 304 - cached file is still valid");
            return cache_valid;
         end if;
      end if;
      Event.emit_debug (high_level, "Failed to retrieve file, RC =" & response_code'Img);
      return retrieval_failed;
   end download_file;


   ---------------------------
   --  using_file_protocol  --
   ---------------------------
   function using_file_protocol (url : String) return Boolean is
   begin
      return Strings.leads (url, "file://");
   end using_file_protocol;


   -----------------------
   --  copy_local_file  --
   -----------------------
   function copy_local_file
     (remote_file_url : String;
      downloaded_file : String) return fetch_result
   is
      function source_file return String
      is
         --  file:// length is 7
         --  Support file://localhost/
         --  silently correct invalid "file://xxx" to "file:///xxx"
         offset : Natural := 6;
      begin
         if Strings.leads (remote_file_url, "file:///") then
            offset := 7;
         elsif Strings.leads (remote_file_url, "file://localhost/") then
            offset := 16;
         end if;
         return remote_file_url (remote_file_url'First + offset .. remote_file_url'Last);
      end;
   begin
      DIR.Copy_File (source_file, downloaded_file);
      return file_downloaded;
   exception
      when others =>
         return retrieval_failed;
   end copy_local_file;


   ------------------------------
   --  download_post_response  --
   ------------------------------
   function download_post_response
     (remote_file_url : String;
      etag_file       : String;
      downloaded_file : String;
      post_body       : String;
      post_body_type  : String) return fetch_result
   is
      temporary_file  : constant String := CAL.randomized_download_target (downloaded_file);

      data : CAL.curldata;
      curlobj : curl_header.CURLX;
      response_code : Long_Integer;
      header_list : curl_header.access_curl_slist := null;
      successful_execution : Boolean;
      post_length  : constant Long_Integer := post_body'Length;
      post_address : constant curl_header.Void_Ptr := post_body'Address;
   begin
      if CAL.target_file_cached (downloaded_file, etag_file) then
         Event.emit_debug (high_level, "Latest " & downloaded_file & " is already cached.");
         return cache_valid;
      end if;

      if not Strings.leads (remote_file_url, "http") then
         Event.emit_debug (high_level, "Protocol does not support POST data: " & remote_file_url);
      end if;

      data.progress := 0;
      data.file_size := 0;
      data.display_pc := False;
      data.etag_file := CAL.ASU.To_Unbounded_String (etag_file);
      begin
         CAL.SIO.Create (data.file_handle, CAL.SIO.Out_File, temporary_file);
      exception
         when others =>
            Event.emit_debug (high_level, "Failed to create " & temporary_file & " for writing");
            return retrieval_failed;
      end;

      curlobj := curl_header.curl_easy_init;
      data.curlobj := curlobj;

      curl_header.set_curl_option (curlobj, curl_header.CURLOPT_NOPROGRESS, True);
      curl_header.set_curl_option (curlobj, curl_header.CURLOPT_TRANSFER_ENCODING, True);
      curl_header.set_curl_option (curlobj, curl_header.CURLOPT_URL, remote_file_url);
      curl_header.set_curl_option (curlobj, curl_header.CURLOPT_WRITEDATA, data'Address);
      curl_header.set_curl_option (curlobj, curl_header.CURLOPT_HEADERDATA, data'Address);
      curl_header.set_write_callback (curlobj, CAL.write_file'Access);
      curl_header.set_header_callback (curlobj, CAL.process_header'Access);
      curl_header.build_header (header_list, "Content-Type: " & post_body_type);

      declare
         verbose        : Boolean;
         agent          : constant String := RCU.config_setting_as_string (RCU.CFG.user_agent);
         no_verify_peer : constant Boolean := ENV.Exists ("SSL_NO_VERIFY_PEER");
         no_verify_host : constant Boolean := ENV.Exists ("SSL_NO_VERIFY_HOSTNAME");
         http_proxy     : constant String := ENV.Value ("HTTP_PROXY", "");
         ssl_key        : constant String := ENV.Value ("SSL_CLIENT_KEY_FILE", "");
         ssl_cert       : constant String := ENV.Value ("SSL_CLIENT_CERT_FILE", "");
         cert_file      : constant String := ENV.Value ("SSL_CA_CERT_FILE", "");
         netrc_file     : constant String := ENV.Value ("NETRC", "");
         set_protocol   : IP_support;
      begin
         if agent /= "" then
            curl_header.set_curl_option (curlobj, curl_header.CURLOPT_USERAGENT, agent);
         end if;
         case Raven.Context.reveal_debug_level is
            when silent | high_level => verbose := False;
            when others => verbose := True;
         end case;
         set_protocol := Raven.Context.reveal_protocol_restriction;
         case set_protocol is
            when no_restriction => null;
            when IPv4_only => curl_header.set_curl_option (curlobj,
                                                           curl_header.CURLOPT_IPRESOLVE,
                                                           curl_header.CURL_IPRESOLVE_V4);
            when IPv6_only => curl_header.set_curl_option (curlobj,
                                                           curl_header.CURLOPT_IPRESOLVE,
                                                           curl_header.CURL_IPRESOLVE_V6);
         end case;
         curl_header.set_curl_option (curlobj, curl_header.CURLOPT_VERBOSE, verbose);
         if no_verify_peer then
            curl_header.set_curl_option (curlobj, curl_header.CURLOPT_SSL_VERIFYPEER, False);
         end if;
         if no_verify_host then
            curl_header.set_curl_option (curlobj, curl_header.CURLOPT_SSL_VERIFYHOST, 0);
         end if;
         if http_proxy /= "" then
            curl_header.set_curl_option (curlobj, curl_header.CURLOPT_PROXY, http_proxy);
         end if;
         if ssl_key /= "" then
            curl_header.set_curl_option (curlobj, curl_header.CURLOPT_SSLKEY, ssl_key);
         end if;
         if ssl_cert /= "" then
            curl_header.set_curl_option (curlobj, curl_header.CURLOPT_SSLCERT, ssl_cert);
         end if;
         if cert_file /= "" then
            curl_header.set_curl_option (curlobj, curl_header.CURLOPT_CAINFO, cert_file);
         end if;
         if netrc_file /= "" then
            curl_header.set_curl_option (curlobj, curl_header.CURLOPT_NETRC_FILE, netrc_file);
         end if;
      end;

      if CAL.found_etag_file (etag_file) and Archive.Unix.file_exists (downloaded_file) then
         declare
            set_etag : constant String := "If-None-Match: " &
              LAT.Quotation & CAL.file_to_string (etag_file) & LAT.Quotation;
         begin
            curl_header.build_header (header_list, set_etag);
         end;
      end if;
      curl_header.set_curl_option (curlobj, curl_header.CURLOPT_HTTPHEADER, header_list);

      curl_header.set_curl_option (curlobj, curl_header.CURLOPT_POST, True);
      curl_header.set_curl_option (curlobj, curl_header.CURLOPT_POSTFIELDS, post_address);
      curl_header.set_curl_option (curlobj, curl_header.CURLOPT_POSTFIELDSIZE, post_length);

      successful_execution := curl_header.execute_curl (curlobj);
      CAL.SIO.Close (data.file_handle);
      curl_header.curl_slist_free_all (header_list);

      if CAL.found_etag_file (etag_file) then
         --  Not all URLs produce etag files
         if not CAL.set_expiration_time (etag_file, data.max_age) then
            Event.emit_debug (high_level, "Failed to set mtime of " & etag_file & " in the future");
            return retrieval_failed;
         end if;
      end if;

      response_code := curl_header.get_info_value_long (curlobj,
                                                        curl_header.CURLINFO_RESPONSE_CODE);
      if response_code = 200 then
         CAL.rename_temporary_file (downloaded_file, temporary_file);
      else
         CAL.remove_temporary_file (temporary_file);
      end if;
      if Archive.Unix.file_exists (downloaded_file) then
         if response_code = 200 then
            Event.emit_debug (high_level, "Received file " & downloaded_file);
            return file_downloaded;
         end if;
         if response_code = 304 then
            Event.emit_debug (high_level, "Response 304 - cached file is still valid");
            return cache_valid;
         end if;
      end if;
      Event.emit_debug (high_level, "Failed to retrieve file, RC =" & response_code'Img);
      return retrieval_failed;

   end download_post_response;

end Raven.Fetch;
