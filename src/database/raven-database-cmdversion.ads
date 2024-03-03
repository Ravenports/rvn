--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

package Raven.Database.Cmdversion is

   function create_rvnindex
     (rvndb              : in out Database.RDB_Connection;
      database_directory : String;
      database_file_path : String;
      rvnindex_file_path : String) return Boolean;

private

   internal_srcfile : constant String := "raven-database-cmdversion.ads";

end Raven.Database.Cmdversion;
