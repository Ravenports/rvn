--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

private with Raven.Metadata;
private with Raven.Database;
private with ThickUCL;
private with Archive.Unpack;

package Raven.Cmd.Install is

   --  Executes install command
   function execute_install_command (comline : Cldata) return Boolean;

private

   package MET renames Raven.Metadata;
   package EXT renames Archive.Unpack;

   --  temporary function; remove when no longer needed
   function currently_unsupported (switch : String) return Boolean;

   --  Handle local archive installation (repository bypass) separately
   function install_single_local_package
     (rdb     : in out Database.RDB_Connection;
      comline : Cldata) return Boolean;


   --  Inserts key data from a single package's metadata as a package locally installed
   function register_single_package
     (rdb            : in out Database.RDB_Connection;
      metatree       : ThickUCL.UclTree;
      file_list      : EXT.file_records.Vector;
      mark_automatic : Boolean;
      force_install  : Boolean) return Boolean;


end Raven.Cmd.Install;
