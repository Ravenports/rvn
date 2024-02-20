--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Characters.Latin_1;
with Ada.Environment_Variables;
with Raven.Event;
with Raven.Context;
with Raven.Cmd.Unset;
with curl_header;
with curl_callbacks;
with Archive.Unix;

package body Raven.Fetch is

   package CAL renames curl_callbacks;
   package RCU renames Raven.Cmd.Unset;
   package LAT renames Ada.Characters.Latin_1;
   package ENV renames Ada.Environment_Variables;

   ---------------------
   --  download_file  --
   ---------------------
   function download_file
     (remote_file_url : String;
      etag_file       : String;
      downloaded_file : String) return fetch_result
   is
      temporary_file  : constant String := CAL.randomized_download_target (downloaded_file);

      data : CAL.curldata;
      curlobj : curl_header.CURLX;
      response_code : Long_Integer;
      header_list : curl_header.access_curl_slist := null;
   begin
      if CAL.target_file_cached (downloaded_file, etag_file) then
         Event.emit_debug (high_level, "Latest " & downloaded_file & " is already cached.");
         return cache_valid;
      end if;

      data.totalsize := 0;
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
      begin
         if agent /= "" then
            curl_header.set_curl_option (curlobj, curl_header.CURLOPT_USERAGENT, agent);
         end if;
         case Raven.Context.reveal_debug_level is
            when silent => verbose := False;
            when others => verbose := True;
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

      if CAL.found_etag_file (etag_file) then
         declare
            set_etag : constant String := "If-None-Match: " &
              LAT.Quotation & CAL.file_to_string (etag_file) & LAT.Quotation;
         begin
            curl_header.build_header (header_list, set_etag);
            curl_header.set_curl_option (curlobj, curl_header.CURLOPT_HTTPHEADER, header_list);
         end;
      end if;

      curl_header.execute_curl (curlobj);
      CAL.SIO.Close (data.file_handle);
      curl_header.curl_slist_free_all (header_list);

      if not CAL.set_expiration_time (etag_file, data.max_age) then
         Event.emit_debug (high_level, "Failed to set mtime of " & etag_file & " in the future");
         return retrieval_failed;
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
         if response_code = 340 then
            Event.emit_debug (high_level, "Response 340 - cached file is still valid");
            return cache_valid;
         end if;
      end if;
      Event.emit_debug (high_level, "Failed to retrieve file, RC =" & response_code'Img);
      return retrieval_failed;
   end download_file;


end Raven.Fetch;
