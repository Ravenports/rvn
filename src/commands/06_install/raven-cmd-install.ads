--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../../License.txt


package Raven.Cmd.Install is

   --  Executes install command
   function execute_install_command (comline : Cldata) return Boolean;

private

   --  temporary function; remove when no longer needed
   function currently_unsupported (switch : String) return Boolean;

end Raven.Cmd.Install;
