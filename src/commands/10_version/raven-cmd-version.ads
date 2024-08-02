--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

private with Raven.Database;

package Raven.Cmd.Version is

   --  Executes version command
   function execute_version_command (comline : Cldata) return Boolean;

private

   DLOAD_FAILED : constant String := "__Download_Failed__";

   type download_type is (release, snapshot, reldate);
   type A_Source is (S_snapshot_index, I_release_index, R_remote_catalog);

   function downloaded_etag_path (downfile : download_type) return String;
   function download_file_path (downfile : download_type) return String;
   function index_database_path (downfile : download_type) return String;
   function index_url (downfile : download_type) return String;
   function cache_directory return String;
   function database_directory return String;
   function latest_release_url return String;

   --  Returns true if the index was successfully downloaded, parsed, and an SQLite db created.
   --  Also returns true if the SQLite db exists and the index cache is present and valid
   function create_index_database (index_type : download_type) return Boolean;

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
     (installed_nsv       : String;
      installed_version   : String;
      source              : A_Source;
      remote_version      : String;
      option_match_status : Boolean;
      option_avoid_status : Boolean;
      option_verbose      : Boolean;
      option_cmp_operator : Character) return Display_Line;

   function compare_against_rvnindex
     (source              : download_type;
      option_match_status : Boolean;
      option_avoid_status : Boolean;
      option_verbose      : Boolean;
      behave_cs           : Boolean;
      behave_exact        : Boolean;
      option_cmp_operator : Character;
      pattern             : String) return Boolean;

   function compare_against_catalog
     (single_repo         : String;
      option_match_status : Boolean;
      option_avoid_status : Boolean;
      option_quiet        : Boolean;
      option_verbose      : Boolean;
      behave_cs           : Boolean;
      behave_exact        : Boolean;
      option_cmp_operator : Character;
      pattern             : String) return Boolean;

   function refresh_catalog
     (single_repo         : String;
      option_quiet        : Boolean) return Boolean;

end Raven.Cmd.Version;
