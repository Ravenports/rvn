--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Pkgtypes;

package Raven.Database.Add is

   function top_level_addition_list
     (db             : RDB_Connection;
      packages       : in out Pkgtypes.Package_Set.Vector;
      pattern        : String;
      override_exact : Boolean;
      select_all     : Boolean) return Boolean;

   procedure gather_packages_affected_by_libchange
     (db             : RDB_Connection;
      old_library    : String;
      affected_list  : in out Pkgtypes.Text_List.Vector);

   procedure collect_installed_files
     (db             : RDB_Connection;
      new_upgrades   : Pkgtypes.Text_List.Vector;
      collection     : in out Pkgtypes.NV_Pairs.Map);

private

   internal_srcfile : constant String := "raven-database-add.adb";
   nsv_formula : constant String := "p.namebase ||'~'|| p.subpackage ||'~'|| p.variant";

   function allow_addition
     (subpackage     : String;
      override_exact : Boolean) return Boolean;

end Raven.Database.Add;
