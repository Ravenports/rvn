--  SPDX-License-Identifier: ISC
--  Reference: /License.txt


private with Raven.Database;

package Raven.Cmd.Check is

   --  Executes check command
   function execute_check_command (comline : Cldata) return Boolean;

private

   procedure check_dependencies
     (db      : Database.RDB_Connection;
      quiet   : Boolean;
      verbose : Boolean);

   procedure check_files
     (db      : Database.RDB_Connection;
      quiet   : Boolean;
      verbose : Boolean);

end Raven.Cmd.Check;
