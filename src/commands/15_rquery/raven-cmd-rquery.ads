--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

private with Raven.Database;

package Raven.Cmd.RQuery is

   --  Executes rquery command
   function execute_rquery_command (comline : Cldata) return Boolean;

end Raven.Cmd.RQuery;
