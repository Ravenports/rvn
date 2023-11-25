--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

package Raven.Cmd.Bahnhof is

   --  Executes the selected command (or help/version) and returns success/failure
   --  for exit code.
   function execute_command (comline : Cldata) return Boolean;

end Raven.Cmd.Bahnhof;
