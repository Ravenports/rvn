--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

package Raven.Cmd.Unset is

   --  This routine covers the case of ravensw executed without a command verb
   --  For example, to get version information or perform the activation status check
   function execute_no_command (comline : Cldata) return Boolean;

   --  Return False if initialization fails
   function initialize_program (comline : Cldata) return Boolean;

private

   --  rvn -v
   function basic_version_info return Boolean;

   --  rvn -vv
   function extended_version_info return Boolean;

   --  rnv -l
   function list_available_commands return Boolean;

   --  part of rvn -vv
   procedure show_repository_info;

   --  rvn --status-check
   function do_status_check return Boolean;

end Raven.Cmd.Unset;
