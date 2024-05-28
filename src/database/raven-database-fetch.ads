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
      destination  : String) return Boolean;

   --  TODO: separate function for "available-updates" goes here

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

   --  subroutine to run individual queries
   --  The result set is added to the existing container
   procedure retrieve_remote_file_metadata
     (db           : RDB_Connection;
      sql          : String;
      bind_one     : String;
      like_match   : Boolean;
      remote_files : in out Remote_Files_Set.Map);

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


end Raven.Database.Fetch;
