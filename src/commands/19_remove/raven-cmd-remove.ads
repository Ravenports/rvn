--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

private with Raven.Database;

package Raven.Cmd.Remove is

   --  Executes remove command (wrapper)
   function execute_remove_command (comline : Cldata) return Boolean;

private

   function execute_remove_command_core
     (rdb     : in out Database.RDB_Connection;
      comline : Cldata) return Boolean;

end Raven.Cmd.Remove;
