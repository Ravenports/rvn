--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

package Raven.Cmd.Usage is

   type precheck_result is (error_found, action_needed, command_pending, nothing_to_do);

   --  Check if error occurred during command line parsing
   --  If not, return true.   If so, show error and dynamic usage.
   function command_line_valid (comline : Cldata) return Boolean;

   --  Usually the command line has to be parsed twice.
   --  This function analyzes how the first parse went.
   --  On error_found and nothing_to_do results, notices/errors are emitted.
   function precheck_command_line (comline : Cldata) return precheck_result;

   --  Prints the pending command with an unrecognized error message
   procedure alert_command_unrecognized (comline : Cldata);

private

   --  Common routine to display error message (before usage).
   procedure display_error (error_msg : String);

   --  Common routine to display usage messages
   procedure display_usage (usage_msg : String; first_line : Boolean);

   --  Common routine to display usage message without progname
   procedure display_usage_multiline (usage_msg : String);

   --  Common routine to display final help suggestion
   procedure display_help_suggestion (command : Command_verb);

   --  Adds a carriage return to standard error stream
   procedure insert_carriage_return;

   --  Break each command into individual routines.  Any additional
   --  validation checks (if no parsing error exists) can be done here.
   function verb_alias   (comline : Cldata) return Boolean;
   function verb_config  (comline : Cldata) return Boolean;
   function verb_create  (comline : Cldata) return Boolean;
   function verb_help    (comline : Cldata) return Boolean;
   function verb_info    (comline : Cldata) return Boolean;
   function verb_install (comline : Cldata) return Boolean;
   function verb_shlib   (comline : Cldata) return Boolean;
   function verb_which   (comline : Cldata) return Boolean;
   function verb_version (comline : Cldata) return Boolean;
   function verb_genrepo (comline : Cldata) return Boolean;
   function verb_catalog (comline : Cldata) return Boolean;
   function verb_clean   (comline : Cldata) return Boolean;
   function verb_shell   (comline : Cldata) return Boolean;
   function verb_query   (comline : Cldata) return Boolean;
   function verb_rquery  (comline : Cldata) return Boolean;
   function verb_stats   (comline : Cldata) return Boolean;
   function verb_search  (comline : Cldata) return Boolean;
   function verb_fetch   (comline : Cldata) return Boolean;
   function verb_remove  (comline : Cldata) return Boolean;
   function verb_autorem (comline : Cldata) return Boolean;
   function verb_check   (comline : Cldata) return Boolean;

end Raven.Cmd.Usage;
