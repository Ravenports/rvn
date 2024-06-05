--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

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

   type refresh_action is (reinstall, upgrade);

   function reinstall_or_upgrade
     (rdb         : in out Database.RDB_Connection;
      action      : refresh_action;
      current_pkg : Pkgtypes.A_Package;
      updated_pkg : String;
      rootdir     : String;
      no_scripts  : Boolean;
      post_report : TIO.File_Type) return Boolean;

end Raven.Install;
