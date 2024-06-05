--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

private with Raven.Database;

package Raven.Cmd.Autoremove is

   --  Executes autoremove command (wrapper)
   function execute_autoremove_command (comline : Cldata) return Boolean;

private

   function execute_autoremove_command_core
     (rdb     : in out Database.RDB_Connection;
      comline : Cldata) return Boolean;

end Raven.Cmd.Autoremove;
