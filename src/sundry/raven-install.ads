--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Text_IO;
with Raven.Pkgtypes;
with Raven.Database;

package Raven.Install is

   --  current_pkg only needs to be an unfinished package + annotations.
   --  This is make the automatic flag and the custom annotations persistent across
   --  upgrades and reinstallations.  rdb must be open and exclusively locked.
   --  rootdir comes from the pre-command install_rootdir value
   --
   --  Step 1. extract metadata from rvn package
   --  Step 2. convert updated_pkg to A_Package structure
   --  Step 3. Overwrite automatic and add any custom annotations
   --  Step 4. Remove current installation (record and files)
   --  Step 5. Extract rvn package
   --  Step 6. Register package

   type refresh_action is (new_install, reinstall, upgrade);

   function reinstall_or_upgrade
     (rdb         : in out Database.RDB_Connection;
      action      : refresh_action;
      current_pkg : Pkgtypes.A_Package;
      updated_pkg : String;
      rootdir     : String;
      no_scripts  : Boolean;
      post_report : Ada.Text_IO.File_Type) return Boolean;

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

end Raven.Install;
