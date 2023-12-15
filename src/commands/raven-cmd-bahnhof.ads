--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

package Raven.Cmd.Bahnhof is

   --  Executes the selected command (or help/version) and returns success/failure
   --  for exit code.
   function execute_command (comline : Cldata) return Boolean;

end Raven.Cmd.Bahnhof;
