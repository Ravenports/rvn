--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

package Raven.Cmd.Usage is

   --  Check if error occurred during command line parsing
   --  If not, return true.   If so, show error and dynamic usage.
   function command_line_valid (comline : Cldata) return Boolean;

private

   --  Common routine to display error message (before usage).
   procedure display_error (error_msg : String);

   --  Common routine to display usage messages
   procedure display_usage (usage_msg : String; first_line : Boolean);

   --  Common routine to display final help suggestion
   procedure display_help_suggestion (command : Command_verb);

   --  Adds a carriage return to standard error stream
   procedure insert_carriage_return;

   --  Break each command into individual routines.  Any additional
   --  validation checks (if no parsing error exists) can be done here.
   function no_command_verb (comline : Cldata) return Boolean;
   function verb_create (comline : Cldata) return Boolean;
   function verb_help (comline : Cldata) return Boolean;
   function verb_info (comline : Cldata) return Boolean;

end Raven.Cmd.Usage;
