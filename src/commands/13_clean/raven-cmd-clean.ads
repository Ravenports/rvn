--  SPDX-License-Identifier: ISC
--  Reference: /License.txt


private with Raven.Database;

package Raven.Cmd.Clean is

   --  Executes clean command
   function execute_clean_command (comline : Cldata) return Boolean;

end Raven.Cmd.Clean;
