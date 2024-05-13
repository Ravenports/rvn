--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

package Raven.Fetch is

   type fetch_result is (cache_valid, file_downloaded, retrieval_failed);


   --  remote_file_url : e.g. https://raw.githubusercontent.com/Ravenports/Ravenports/ ..
   --                                master/Mk/Misc/rvnindex.txt
   --  etag_file       : e.g. /var/cache/rvn/version/rvnindex.etag
   --  downloaded_file : e.g. /var/cache/rvn/version/rvnindex.txt
   --  remote_repo     : e.g. False
   --  remote_protocol : e.g. no_restrictions

   function download_file
     (remote_file_url : String;
      etag_file       : String;
      downloaded_file : String;
      remote_repo     : Boolean := False;
      remote_protocol : IP_support := no_restriction) return fetch_result;

end Raven.Fetch;
