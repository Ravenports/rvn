--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Pkgtypes;

package Raven.Database.Annotate is

   procedure annotate_packages
     (db       : RDB_Connection;
      tag      : String;
      note     : String;
      packages : Pkgtypes.Package_Set.Vector);

   procedure remove_annotations
     (db       : RDB_Connection;
      tag      : String;
      packages : Pkgtypes.Package_Set.Vector);

private

   internal_srcfile : constant String := "raven-database-annotate.adb";

end Raven.Database.Annotate;
