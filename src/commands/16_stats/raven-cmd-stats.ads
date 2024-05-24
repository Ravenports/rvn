--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

private with Raven.Database;

package Raven.Cmd.Stats is

   --  Executes stats command
   function execute_stats_command (comline : Cldata) return Boolean;

private

   rdb : Database.RDB_Connection;

end Raven.Cmd.Stats;
