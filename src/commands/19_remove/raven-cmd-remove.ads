--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

private with Ada.Containers.Vectors;
private with Raven.Database;


package Raven.Cmd.Remove is

   --  Executes remove command
   function execute_remove_command (comline : Cldata) return Boolean;

private

   package Purge_Order_Crate is new Ada.Containers.Vectors
     (Element_Type => Natural,
      Index_Type   => Natural);

   --  Delete in alphabetical order of NSV identifier
   procedure determine_purge_order
     (purge_list  : Pkgtypes.Package_Set.Vector;
      purge_order : in out Purge_Order_Crate.Vector);

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

end Raven.Cmd.Remove;
