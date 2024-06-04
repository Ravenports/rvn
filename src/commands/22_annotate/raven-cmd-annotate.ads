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

end Raven.Cmd.Annotate;
