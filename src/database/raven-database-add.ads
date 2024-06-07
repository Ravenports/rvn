--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Pkgtypes;

package Raven.Database.Add is

   function top_level_addition_list
     (db             : RDB_Connection;
      packages       : in out Pkgtypes.Package_Set.Vector;
      pattern        : String;
      override_exact : Boolean) return Boolean;

private

   internal_srcfile : constant String := "raven-database-add.adb";
   nsv_formula : constant String := "p.namebase ||'-'|| p.subpackage ||'-'|| p.variant";

   function allow_addition (subpackage : String) return Boolean;

end Raven.Database.Add;
