--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

private with Raven.Database;
private with Raven.Pkgtypes;

package Raven.Cmd.Version is

   --  Executes version command
   function execute_version_command (comline : Cldata) return Boolean;

private

   rdb : Database.RDB_Connection;

end Raven.Cmd.Version;
