--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Pkgtypes;

package Raven.Event is

   procedure emit_error          (message : String);
   procedure emit_notice         (message : String);
   procedure emit_message        (message : String);
   procedure emit_premessage     (message : String);
   procedure emit_no_local_db;
   procedure emit_no_remote_db;

   procedure emit_debug
     (debug_level : A_Debug_Level;
      debug_msg   : String);

   procedure emit_errno
     (err_function : String;
      err_argument : String;
      err_number   : Integer);

   procedure emit_install_begin (pkg : Pkgtypes.A_Package);
   procedure emit_install_end   (pkg : Pkgtypes.A_Package);
   procedure emit_extract_begin (pkg : Pkgtypes.A_Package);
   procedure emit_extract_end   (pkg : Pkgtypes.A_Package);
   procedure emit_remove_begin  (pkg : Pkgtypes.A_Package);
   procedure emit_remove_end    (pkg : Pkgtypes.A_Package);
   procedure emit_upgrade_begin (pkg : Pkgtypes.A_Package; newpkg : Pkgtypes.A_Package);
   procedure emit_upgrade_end   (pkg : Pkgtypes.A_Package; newpkg : Pkgtypes.A_Package);

   procedure emit_fetch_begin    (url : String; rvnsize : Pkgtypes.Package_Size);
   procedure emit_fetch_finished
     (url     : String;
      rvnsize : Pkgtypes.Package_Size;
      result  : String);

private

   --  warnx prints the message to stdout verbatim
   procedure warnx (verbatim_message : String);

   --  Push data through a connected pipe
   procedure pipe_event (json_message : String);


end Raven.Event;
