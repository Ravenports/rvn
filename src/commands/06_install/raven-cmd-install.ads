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

   rdb : Database.RDB_Connection;

   --  temporary function; remove when no longer needed
   function currently_unsupported (switch : String) return Boolean;

   --  Handle local archive installation (repository bypass) separately
   function install_single_local_package (comline : Cldata) return Boolean;


   --  Inserts key data from a single package's metadata as a package locally installed
   function register_single_package
     (metatree       : ThickUCL.UclTree;
      file_list      : EXT.file_records.Vector;
      mark_automatic : Boolean;
      force_install  : Boolean) return Boolean;


   --  Prior to this routine many things need to have been done:
   --  * archive_path verified to be an existing regular file
   --  * conflict check (conflicts overridden by --forced)
   --  * interactive user confirmations
   --  * plan to handle dependencies (affected by --ignore-missing and --recursive)
   --  * This routine populates the file list.
   --  This routine installs the files from the RVN archive into the root directory
   --    (affected by rvn -[r|c]).   If dry-run selected no installation will actually occur.
   --  Returns true if extraction was successful.
   function install_files_from_archive
     (archive_path      : String;
      root_directory    : String;
      inhibit_scripts   : Boolean;
      be_silent         : Boolean;
      dry_run_only      : Boolean;
      upgrading         : Boolean) return Boolean;




end Raven.Cmd.Install;
