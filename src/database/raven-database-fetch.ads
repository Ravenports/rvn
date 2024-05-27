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

end Raven.Database.Fetch;
