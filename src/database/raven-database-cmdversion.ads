--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Pkgtypes;

package Raven.Database.Cmdversion is

   function create_rvnindex
     (database_directory : String;
      database_file_path : String;
      rvnindex_file_path : String) return Boolean;

   procedure map_nsv_to_rvnindex_version
     (database_directory : String;
      database_file_path : String;
      version_map        : in out Pkgtypes.NV_Pairs.Map);

   procedure map_nsv_to_local_version
     (db                 : RDB_Connection;
      behave_cs          : Boolean;
      behave_exact       : Boolean;
      pattern            : String;
      version_map        : in out Pkgtypes.NV_Pairs.Map);

private

   internal_srcfile : constant String := "raven-database-cmdversion.ads";

end Raven.Database.Cmdversion;
