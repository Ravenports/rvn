--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

private with Raven.Pkgtypes;
private with Raven.Database;

package Raven.Cmd.Annotate is

      --  Executes annotate command
   function execute_annotate_command (comline : Cldata) return Boolean;

private

   procedure display_tags
     (db               : Database.RDB_Connection;
      shallow_packages : Pkgtypes.Package_Set.Vector;
      match_tag        : String);

   procedure delete_tags
     (db               : Database.RDB_Connection;
      shallow_packages : Pkgtypes.Package_Set.Vector;
      match_tag        : String;
      quiet            : Boolean);

   function format_removal_order (counter : Natural) return String;

   function granted_permission_to_proceed (this_task : String) return Boolean;

end Raven.Cmd.Annotate;
