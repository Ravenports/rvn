--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Pkgtypes;

package Raven.Database.Remove is

   function top_level_deletion_list
     (db             : in out RDB_Connection;
      packages       : in out Pkgtypes.Package_Set.Vector;
      pattern        : String;
      all_packages   : Boolean;
      override_exact : Boolean;
      force          : Boolean) return Boolean;

private

   internal_srcfile : constant String := "raven-database-remove.adb";
   nsv_formula : constant String := "p.namebase ||'-'|| p.subpackage ||'-'|| p.variant";

end Raven.Database.Remove;
