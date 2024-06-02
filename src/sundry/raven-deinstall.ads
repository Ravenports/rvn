--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Text_IO;
with Raven.Pkgtypes;
with Archive.Whitelist;

package Raven.Deinstall is

   package TIO renames Ada.Text_IO;
   package ARW renames Archive.Whitelist;

   procedure deinstall_extracted_package
     (installed_package   : Pkgtypes.A_Package;
      verify_digest_first : Boolean;
      quiet               : Boolean;
      post_report         : TIO.File_Type);

private

   procedure run_shell_scripts
     (phase       : ARW.package_phase;
      the_package : Pkgtypes.A_Package;
      upgrading   : Boolean;
      msg_outfile : String);

   procedure run_lua_scripts
     (phase       : ARW.package_phase;
      the_package : Pkgtypes.A_Package;
      upgrading   : Boolean;
      msg_outfile : String);

   procedure prune_empty_directories
     (the_package : Pkgtypes.A_Package);

end Raven.Deinstall;
