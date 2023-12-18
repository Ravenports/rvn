--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Pkgtypes;

package Raven.Database.Query is

   --  Returns 0 if the package is not registered
   --  Returns package ID if the package is installed
   function package_installed
     (db         : in out RDB_Connection;
      namebase   : String;
      subpackage : String;
      variant    : String) return Pkgtypes.Package_ID;

private

   internal_srcfile : constant String := "raven-database-query.adb";

end Raven.Database.Query;
