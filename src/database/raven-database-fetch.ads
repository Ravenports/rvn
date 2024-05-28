--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Pkgtypes;

private with Ada.Containers.Hashed_Maps;
private with Raven.Miscellaneous;
private with Raven.Strings;

package Raven.Database.Fetch is

   --  This is the heart of the rvn fetch command
   --  It is also called by rvn install
   --  Returns False if an error was encountered
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
      single_repo  : String) return Boolean;

   --  Given open connections to the local and catalog databases, this subroutine
   --  populates "package_list", a vector of nsv strings that represent packages that
   --  are out of data as compared to the catalog.-
   procedure list_of_upgradeable_packages
     (ldb          : RDB_Connection;
      rdb          : RDB_Connection;
      package_list : in out Pkgtypes.Text_List.Vector);

private

   internal_srcfile : constant String := "raven-database-fetch.adb";

   subtype short_digest is string (1 .. 10);

   type A_Remote_File is
      record
         nsvv     : Text;
         flatsize : Pkgtypes.Package_Size;
         rvnsize  : Pkgtypes.Package_Size;
      end record;

   package Remote_Files_Set is new Ada.Containers.Hashed_Maps
     (Key_Type        => short_digest,
      Element_Type    => A_Remote_File,
      Hash            => Raven.Miscellaneous.map_hash,
      Equivalent_Keys => Strings.equivalent);

   package Tracked_Set is new Ada.Containers.Hashed_Maps
     (Key_Type        => Text,
      Element_Type    => Boolean,
      Hash            => Raven.Miscellaneous.map_hash,
      Equivalent_Keys => Strings.equivalent);

   type download_result is (verified_download, failed_download, failed_verification);

   --  subroutine to run individual queries
   --  The result set is added to the existing container
   procedure retrieve_remote_file_metadata
     (db           : RDB_Connection;
      sql          : String;
      bind_one     : String;
      like_match   : Boolean;
      remote_files : in out Remote_Files_Set.Map;
      package_seen : in out Tracked_Set.Map);

   --  subroutine to run individual queries on the packages dependencies
   --  It sets the retrieved packages as "seen" and makes the initial dependency queue to
   --  recurse into.
   procedure retrieve_dependencies_by_pattern
     (db           : RDB_Connection;
      sql          : String;
      bind_one     : String;
      like_match   : Boolean;
      package_seen : Tracked_Set.Map;
      depend_queue : in out Pkgtypes.Text_List.Vector);

   --  subroutine to check the given package identified by nsv for additional dependencies
   --  to recursively scan
   procedure retrieve_dependency_by_nsv
     (db           : RDB_Connection;
      base_dep_sql : String;
      nsv          : String;
      package_seen : Tracked_Set.Map;
      depend_queue : in out Pkgtypes.Text_List.Vector);

   --  Given nsv, add new entry to the download list.
   procedure insert_into_download_list
     (db           : RDB_Connection;
      base_sql     : String;
      nsv          : String;
      remote_files : in out Remote_Files_Set.Map;
      package_seen : in out Tracked_Set.Map);

   --  Checks to see if the file or symlink link with filename <nsvv>.rvn exists
   --  in cache alternatively the output file.  If it does, remove it from the list.
   --  All that matters is that the version matches.  If the person wants a hash match
   --  they can use "rvn clean" command first.
   procedure prune_remote_files_list
     (remote_files  : in out Remote_Files_Set.Map;
      destination   : String);

   --  organize the digest keys to reflect alphabetical download order
   procedure sort_queue
     (remote_files   : Remote_Files_Set.Map;
      download_order : in out Pkgtypes.Text_List.Vector);

   --  If not in quiet mode, show the list of proposed downloads
   procedure show_proposed_queue
     (remote_files   : Remote_Files_Set.Map;
      download_order : Pkgtypes.Text_List.Vector;
      behave_quiet   : Boolean);

   --  Returns string 0.0% .. 100.0%
   function percentage (numerator, denominator : Pkgtypes.Package_Size) return string;

   --  if < 10,000, format as "<pad><number>."
   --  10,000 and over, format as "<number>"
   --  always 5 characters long
   function format_download_order (counter : Natural) return String;

   --  Returns true if allowed to proceed with download.
   --  quiet mode implictly grants permission
   function granted_permission_to_proceed (quiet : Boolean) return Boolean;

   --  Returns true if all files are successfully downloaded
   function download_packages
     (remote_files   : Remote_Files_Set.Map;
      download_order : Pkgtypes.Text_List.Vector;
      behave_quiet   : Boolean;
      send_to_cache  : Boolean;
      destination    : String;
      single_repo    : String) return Boolean;

   --  Individual package download
   function download_package
     (remote_url     : Text;
      remote_proto   : IP_support;
      remote_file    : A_Remote_File;
      digest10       : short_digest;
      destination    : String;
      send_to_cache  : Boolean;
      behave_quiet   : Boolean;
      file_counter   : Natural;
      total_files    : Natural) return download_result;

   --  If destination is blank, set it to RVN_CACHEDIR, otherwise pass it through
   function translate_destination (destination : String) return String;

   --  Returns true if <file_url> exists and the first 10 characters of the digest matches
   --  the digest10 input.
   function verify_checksum (file_url : String; digest10 : short_digest) return Boolean;

   --  Generic all packages nsv -> version associative array
   procedure map_nsv_to_version
     (db          : RDB_Connection;
      version_map : in out Pkgtypes.NV_Pairs.Map);

end Raven.Database.Fetch;
