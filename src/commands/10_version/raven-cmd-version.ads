--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

private with Raven.Database;
private with Raven.Pkgtypes;

package Raven.Cmd.Version is

   --  Executes version command
   function execute_version_command (comline : Cldata) return Boolean;

private

   rdb : Database.RDB_Connection;

   DLOAD_FAILED : constant String := "__Download_Failed__";

   type download_type is (release, snapshot, reldate);

   function downloaded_etag_path (downfile : download_type) return String;
   function download_file_path (downfile : download_type) return String;
   function index_database_path (downfile : download_type) return String;
   function index_url (downfile : download_type) return String;
   function cache_directory return String;
   function database_directory return String;
   function latest_release_url return String;

   --  repology_db   : constant String := "rvnindex.sqlite";
   --  snapshot_db   : constant String := "snapshot.sqlite";
   --  repology_json : constant String := "rvnindex.json";
   --  snapshot_json : constant String := "snapshot.json";

   --  rvn version -t <pkg1> <pkg2>
   function do_testversion (pkgname1, pkgname2 : String) return Boolean;

   --  rvn version -T <pkgname> <pattern>
   function do_testpattern
     (pkgname : String;
      pattern : String;
      hyphen1 : Boolean;
      hyphen2 : Boolean) return Boolean;

   type Display_Line is
      record
         identifier : Text;
         comparison : Character;
         extra_info : Text;
         valid      : Boolean;
      end record;

   package Line_Crate is new CON.Vectors
     (Element_Type => Display_Line,
      Index_Type   => Natural);

   --  output
   function print_version
     (pkg_version         : String;
      pkg_nsv             : String;
      source              : String;
      version             : String;
      option_match_status : Boolean;
      option_avoid_status : Boolean;
      option_verbose      : Boolean;
      option_cmp_operator : Character) return Display_Line;

   --  rvn -R [-l limchar | -L limchar] [-n pkgname] [-r reponame] [-e|-g|-x] pattern
   --  function do_remote_index
   --    (match_char  : Character;
   --     not_char    : Character;
   --     match       : Database.Match_Behavior;
   --     pattern     : String;
   --     matchname   : String;
   --     auto_update : Boolean;
   --     quiet       : Boolean;
   --     reponame    : String) return Boolean;


   --  rvn -I [-l limchar | -L limchar] [-n pkgname] [-e|-g|-x] pattern
   --     function do_conspiracy_index
   --       (match_char  : Character;
   --        not_char    : Character;
   --        match       : PkgDB.T_match;
   --        pattern     : String;
   --        matchorigin : String;
   --        matchname   : String) return Boolean;

end Raven.Cmd.Version;
