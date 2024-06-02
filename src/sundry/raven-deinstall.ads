--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Text_IO;
with Ada.Containers.Vectors;
with Raven.Pkgtypes;
with Raven.Database;
with Archive.Whitelist;

package Raven.Deinstall is

   package TIO renames Ada.Text_IO;
   package ARW renames Archive.Whitelist;

   package Purge_Order_Crate is new Ada.Containers.Vectors
     (Element_Type => Natural,
      Index_Type   => Natural);

   --  Delete in alphabetical order of NSV identifier
   procedure determine_purge_order
     (purge_list  : Pkgtypes.Package_Set.Vector;
      purge_order : in out Purge_Order_Crate.Vector);

   procedure deinstall_extracted_package
     (installed_package   : Pkgtypes.A_Package;
      verify_digest_first : Boolean;
      quiet               : Boolean;
      inhibit_scripts     : Boolean;
      post_report         : TIO.File_Type);

   function format_removal_order (counter : Natural) return String;

   procedure show_proposed_queue
     (purge_list     : Pkgtypes.Package_Set.Vector;
      purge_order    : Purge_Order_Crate.Vector;
      behave_quiet   : Boolean;
      dryrun         : Boolean);

   function granted_permission_to_proceed
     (quiet : Boolean) return Boolean;

   procedure remove_packages_in_order
     (rdb            : Database.RDB_Connection;
      purge_list     : Pkgtypes.Package_Set.Vector;
      purge_order    : Purge_Order_Crate.Vector;
      skip_verify    : Boolean;
      skip_scripts   : Boolean;
      quiet          : Boolean);

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
