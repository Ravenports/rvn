--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Pkgtypes;

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
      select_renew : Boolean;
      destination  : String) return Boolean;


end Raven.Database.Fetch;
