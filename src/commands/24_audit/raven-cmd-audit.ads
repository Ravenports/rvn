--  SPDX-License-Identifier: ISC
--  Reference: /License.txt


private with Raven.Database;

package Raven.Cmd.Audit is

   --  Executes audit command
   function execute_audit_command (comline : Cldata) return Boolean;

end Raven.Cmd.Audit;
