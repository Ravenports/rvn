--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

package Raven.Event is

   procedure emit_error          (message : String);
   procedure emit_notice         (message : String);
   procedure emit_message        (message : String);
   procedure emit_premessage     (message : String);
   procedure emit_no_local_db;

   procedure emit_debug
     (debug_level : A_Debug_Level;
      debug_msg   : String);

   procedure emit_errno
     (err_function : String;
      err_argument : String;
      err_number   : Integer);

   procedure emit_install_begin
     (namebase   : String;
      subpackage : String;
      variant    : String;
      version    : String);

   procedure emit_install_end
     (namebase   : String;
      subpackage : String;
      variant    : String;
      version    : String;
      message    : String);

private

   --  warnx prints the message to stdout verbatim
   procedure warnx (verbatim_message : String);

   --  Push data through a connected pipe
   procedure pipe_event (json_message : String);


end Raven.Event;
