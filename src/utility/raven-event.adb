--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Text_IO;
with Raven.Unix;
with Raven.Context;
with Raven.Strings; use Raven.Strings;

package body Raven.Event is

   package TIO renames Ada.Text_IO;

   ------------------
   --  pipe_event  --
   ------------------
   procedure pipe_event (json_message : String) is
   begin
      if Unix.file_connected (Context.reveal_event_pipe) then
         Unix.push_to_event_pipe (Context.reveal_event_pipe, json_message);
      end if;
   end pipe_event;


   ----------------------
   --  check_progress  --
   ----------------------
   procedure check_progress is
   begin
      --  Placeholder in the event progressbar is brought back
      null;
   end check_progress;


   -------------
   --  warnx  --
   -------------
   procedure warnx (verbatim_message : String) is
   begin
      TIO.Put_Line (TIO.Standard_Error, verbatim_message);
   end warnx;


   -------------------
   --  emit_notice  --
   -------------------
   procedure emit_notice (message : String)
   is
      jmsg : constant String := json_object
        (CC
           (json_pair ("type", "NOTICE"),
            json_objectpair ("data", json_pair ("msg", message))));
   begin
      check_progress;
      pipe_event (jmsg);
      TIO.Put_Line (message);
   end emit_notice;


   ------------------
   --  emit_error  --
   ------------------
   procedure emit_error (message : String)
   is
      jmsg : constant String := json_object
        (CC
           (json_pair ("type", "ERROR"),
            json_objectpair ("data", json_pair ("msg", message))));
   begin
      check_progress;
      pipe_event (jmsg);
      warnx (message);
   end emit_error;


   --------------------
   --  emit_message  --
   --------------------
   procedure emit_message (message : String)
   is
      --  Do not send messages to event pipe
   begin
      check_progress;
      TIO.Put_Line (message);
   end emit_message;


   ------------------
   --  emit_debug  --
   ------------------
   procedure emit_debug (debug_level : A_Debug_Level;
                         debug_msg   : String)
   is
      --  Do not send debug events to event pipe
      show_message : Boolean := False;
   begin
      check_progress;
      case Context.reveal_debug_level is
         when low_level =>
            show_message := True;
         when moderate =>
            case debug_level is
               when moderate | high_level | silent => show_message := True;
               when low_level => null;
            end case;
         when high_level =>
            case debug_level is
               when high_level | silent => show_message := True;
               when low_level | moderate => null;
            end case;
         when silent =>
            case debug_level is
               when silent => show_message := True;  -- nothing should be coded this way
               when low_level | moderate | high_level => null;
            end case;
      end case;

      if show_message then
         declare
            lev : constant String := int2str (A_Debug_Level'Pos(debug_level));
            pid : constant String := int2str (Integer (Unix.getpid));
         begin
            warnx ("DBG(" & lev & ")[" & pid & "]> " & debug_msg);
         end;
      end if;
   end emit_debug;


   ------------------
   --  emit_errno  --
   ------------------
   procedure emit_errno (err_function : String;
                         err_argument : String;
                         err_number   : Integer)
   is
      info : constant String := err_function & '(' & err_argument & "): " &
                                Unix.strerror (err_number);
      jmsg : constant String := json_object
        (CC
           (json_pair ("type", "ERROR"),
            json_objectpair ("data",
              CC (json_pair ("msg", info),
                  json_pair ("errno", int2str (err_number))))));
   begin
      check_progress;
      pipe_event (jmsg);
      warnx (info);
   end emit_errno;


   ------------------------
   --  emit_no_local_db  --
   ------------------------
   procedure emit_no_local_db
   is
      jmsg : constant String := json_object
        (CC
           (json_pair ("type", "ERROR_NOLOCALDB"),
            json_objectpair ("data", "")));
   begin
      check_progress;
      pipe_event (jmsg);
      warnx ("Local package database nonexistent!");
   end emit_no_local_db;


   --------------------------
   --  emit_install_begin  --
   --------------------------
   procedure emit_install_begin (namebase   : String;
                                 subpackage : String;
                                 variant    : String;
                                 version    : String)
   is
      nsv : constant String := namebase & "-" & subpackage & "-" & variant;
      jmsg : constant String := json_object
        (CC
           (json_pair ("type", "INFO_INSTALL_BEGIN"),
            json_objectpair ("data",
              CC (json_pair ("pkgname", nsv),
                  json_pair ("pkgversion", version)))));
   begin
      check_progress;
      pipe_event (jmsg);
      TIO.Put_Line ("Install package start: " & nsv & "-" & version);
   end emit_install_begin;


   --------------------------
   --  emit_install_begin  --
   --------------------------
   procedure emit_install_end (namebase   : String;
                               subpackage : String;
                               variant    : String;
                               version    : String;
                               message    : String)
   is
      nsv : constant String := namebase & "-" & subpackage & "-" & variant;
      jmsg : constant String := json_object
        (CC
           (json_pair ("type", "INFO_INSTALL_FINISHED"),
            json_objectpair ("data",
              CC (CC (
                json_pair ("pkgname", nsv),
                json_pair ("pkgversion", version)),
                json_pair ("message", message)))));
   begin
      check_progress;
      pipe_event (jmsg);
      TIO.Put_Line ("Install package end  : " & nsv & "-" & version & "(" & message & ")");
   end emit_install_end;

end Raven.Event;
