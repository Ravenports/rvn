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
      behave_lead  : Boolean;
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

   --  Emits notice of libraries required, one line per library
   procedure print_libraries_required
     (db     : RDB_Connection;
      prefix : String;
      pkgid  : Pkgtypes.Package_ID);

   --  Emits notice of libraries provided, one line per library
   procedure print_libraries_provided
     (db     : RDB_Connection;
      prefix : String;
      pkgid  : Pkgtypes.Package_ID);

   --  Emits notice of dependencies, one line per library
   procedure print_dependencies
     (db     : RDB_Connection;
      prefix : String;
      pkgid  : Pkgtypes.Package_ID);

   --  Emits notice of reverse dependencies, one line per library
   procedure print_reverse_dependencies
     (db     : RDB_Connection;
      prefix : String;
      pkgid  : Pkgtypes.Package_ID);

   --  Emits notice of package annotations, one line per note
   procedure print_annotations
     (db     : RDB_Connection;
      prefix : String;
      pkgid  : Pkgtypes.Package_ID);

   --  Emits notice of package options, one line per option
   procedure print_options
     (db     : RDB_Connection;
      prefix : String;
      pkgid  : Pkgtypes.Package_ID);

private

   internal_srcfile : constant String := "raven-database-search.adb";

   procedure generic_multiline
     (db     : RDB_Connection;
      pkgid  : Pkgtypes.Package_ID;
      func   : String;
      prefix : String;
      sql    : String);

   procedure generic_multiline_map
     (db     : RDB_Connection;
      pkgid  : Pkgtypes.Package_ID;
      func   : String;
      prefix : String;
      sql    : String);

end Raven.Database.Search;
