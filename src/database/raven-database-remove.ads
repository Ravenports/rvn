--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Pkgtypes;

private with Ada.Containers.Hashed_Maps;
private with Raven.Miscellaneous;
private with Raven.Strings;

package Raven.Database.Remove is

   function top_level_deletion_list
     (db             : RDB_Connection;
      packages       : in out Pkgtypes.Package_Set.Vector;
      pattern        : String;
      all_packages   : Boolean;
      override_exact : Boolean;
      force          : Boolean) return Boolean;

   procedure recursive_removal
     (db             : RDB_Connection;
      top_packages   : Pkgtypes.Package_Set.Vector;
      purge_list     : in out Pkgtypes.Package_Set.Vector;
      force          : Boolean);

   procedure drop_package_with_cascade
     (db : RDB_Connection;
      id : Pkgtypes.Package_ID);

   function autoremoval_list
     (db             : RDB_Connection;
      packages       : in out Pkgtypes.Package_Set.Vector) return Boolean;

   procedure prune_candidates_with_reverse_deps
     (db             : RDB_Connection;
      top_packages   : Pkgtypes.Package_Set.Vector;
      purge_list     : in out Pkgtypes.Package_Set.Vector);

private

   internal_srcfile : constant String := "raven-database-remove.adb";
   nsv_formula : constant String := "p.namebase ||'-'|| p.subpackage ||'-'|| p.variant";

   package ID_Set is new Ada.Containers.Hashed_Maps
     (Key_Type        => Text,
      Element_Type    => Raven.Pkgtypes.Package_ID,
      Hash            => Raven.Miscellaneous.map_hash,
      Equivalent_Keys => Strings.equivalent,
      "="             => Raven.Pkgtypes."=");

   procedure gather_reverse_dependencies
     (db             : RDB_Connection;
      rdependencies  : in out ID_Set.Map;
      target_nsv     : Text);

end Raven.Database.Remove;
