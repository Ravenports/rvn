--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

private with Raven.Database;

package Raven.Cmd.Query is

   --  Executes query command
   function execute_query_command (comline : Cldata) return Boolean;

end Raven.Cmd.Query;
