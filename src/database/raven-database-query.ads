--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Pkgtypes;
with Archive.Unpack;

package Raven.Database.Query is

   --  Returns 0 if the package is not registered
   --  Returns package ID if the package is installed
   function package_installed
     (db         : in out RDB_Connection;
      namebase   : String;
      subpackage : String;
      variant    : String) return Pkgtypes.Package_ID;

   --  Populates a container of installed files given a package ID
   procedure get_package_files
     (db         : in out RDB_Connection;
      pkg_id     : Pkgtypes.Package_ID;
      files      : in out Archive.Unpack.file_records.Vector);

   --  Populates a container of truncates packages (only data from main table, no joins)
   procedure rvn_which
     (db         : in out RDB_Connection;
      query_path : String;
      use_glob   : Boolean;
      packages   : in out Pkgtypes.Package_Set.Vector);

private

   internal_srcfile : constant String := "raven-database-query.adb";

end Raven.Database.Query;
