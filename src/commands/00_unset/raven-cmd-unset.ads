--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Configuration;
private with ThickUCL;

package Raven.Cmd.Unset is

   package CFG renames Raven.Configuration;

   --  This routine covers the case of ravensw executed without a command verb
   --  For example, to get version information or perform the activation status check
   function execute_no_command (comline : Cldata) return Boolean;

   --  Set configuration, context, environment
   --  The event pipe opening is best effort.  It's failure is not fatal.
   procedure initialize_program (comline : Cldata);

   --  Close any open file descriptors
   procedure finalize_program;

   --  Returns empty string if alias is not defined, otherwise it
   --  returns the definition of the alias
   function alias_definition (alias : String) return String;

   --  Return configurating setting value (string)
   function config_setting (setting : CFG.Configuration_Item) return String;

   --  Return configurating setting value (Boolean)
   function config_setting (setting : CFG.Configuration_Item) return Boolean;

   --  Return configurating setting value (int64)
   function config_setting (setting : CFG.Configuration_Item) return int64;

   --  Return configuration setting as string.  Booleans and integers are converted.
   --  arrays and objects are concatenated with null characters.
   function config_setting_as_string (setting : CFG.Configuration_Item) return String;

   --  Return configuration object key value as a string (for example, an alias)
   function config_setting_map_value
     (setting : CFG.Configuration_Item;
      map_key : String;
      success : in out Boolean) return String;

   --  Tells the exit routine not to set the exit status again.
   function exit_status_already_set return Boolean;

   --  This indicates that the command set the exit code already
   procedure override_exit_status;

private

   program_configuration : ThickUCL.UclTree;
   sys_exit_overridden   : Boolean := False;

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
