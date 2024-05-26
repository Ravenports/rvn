--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Pkgtypes;

package Raven.Database.Search is

   --  default search method is regex
   --  only one of behave_* can be true.  All can be false.
   --  only one of s_* can be true.  All can be false (means defaulting to namebase field)
   procedure rvn_core_search
     (db           : RDB_Connection;
      srch_pattern : String;
      behave_glob  : Boolean;
      behave_exact : Boolean;
      behave_cs    : Boolean;
      s_comment    : Boolean;
      s_desc       : Boolean;
      s_nsv        : Boolean;
      packages     : in out Pkgtypes.Package_Set.Vector);

   --  Emits notice of prefix followed by categories separated by a space.
   --  If there are no categories, nothing is printed.
   procedure print_categories
     (db     : RDB_Connection;
      prefix : String;
      pkgid  : Pkgtypes.Package_ID);

   --  Emits notice of prefix followed by licenses separated by a space *
   --  If there are no licenses, nothing is printed.
   --  if logic is "dual" or "multi", that will be printed after the prefix.
   procedure print_licenses
     (db     : RDB_Connection;
      prefix : String;
      pkgid  : Pkgtypes.Package_ID;
      logic  : Pkgtypes.License_Logic);

private

   internal_srcfile : constant String := "raven-database-search.adb";

end Raven.Database.Search;
