--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

package Raven.Event is

   procedure emit_error          (message : String);
   procedure emit_notice         (message : String);
   procedure emit_message        (message : String);
   procedure emit_no_local_db;

   procedure emit_debug
     (debug_level : A_Debug_Level;
      debug_msg   : String);

   procedure emit_errno
     (err_function : String;
      err_argument : String;
      err_number   : Integer);

private

   --  warnx prints the message to stdout verbatim
   procedure warnx (verbatim_message : String);

   --  Push data through a connected pipe
   procedure pipe_event (json_message : String);

   -- placeholder
   procedure check_progress;

end Raven.Event;
