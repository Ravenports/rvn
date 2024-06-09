--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Text_IO;
with Raven.Unix;
with Raven.Context;
with ThickUCL.Emitter;
with Ucl;
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
      jmsg : ThickUCL.UclTree;
   begin
      jmsg.insert ("type", "NOTICE");
      jmsg.start_object ("data");
      jmsg.insert ("msg", message);
      jmsg.close_object;
      pipe_event (ThickUCL.Emitter.emit_compact_ucl (jmsg, True));
      TIO.Put_Line (message);
   end emit_notice;


   ------------------
   --  emit_error  --
   ------------------
   procedure emit_error (message : String)
   is
      jmsg : ThickUCL.UclTree;
   begin
      jmsg.insert ("type", "ERROR");
      jmsg.start_object ("data");
      jmsg.insert ("msg", message);
      jmsg.close_object;
      pipe_event (ThickUCL.Emitter.emit_compact_ucl (jmsg, True));
      warnx (message);
   end emit_error;


   --------------------
   --  emit_message  --
   --------------------
   procedure emit_message (message : String)
   is
      --  Do not send messages to event pipe
   begin
      TIO.Put_Line (message);
   end emit_message;


   -----------------------
   --  emit_premessage  --
   -----------------------
   procedure emit_premessage (message : String)
   is
   begin
      TIO.Put (message);
   end emit_premessage;


   ------------------
   --  emit_debug  --
   ------------------
   procedure emit_debug (debug_level : A_Debug_Level;
                         debug_msg   : String)
   is
      --  Do not send debug events to event pipe
      show_message : Boolean := False;
   begin
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
      jmsg : ThickUCL.UclTree;
      info : constant String := err_function & '(' & err_argument & "): " &
                                Unix.strerror (err_number);
   begin
      jmsg.insert ("type", "ERROR");
      jmsg.start_object ("data");
      jmsg.insert ("msg", info);
      jmsg.insert ("errno", Ucl.ucl_integer (err_number));
      jmsg.close_object;
      pipe_event (ThickUCL.Emitter.emit_compact_ucl (jmsg, True));
      warnx (info);
   end emit_errno;


   ------------------------
   --  emit_no_local_db  --
   ------------------------
   procedure emit_no_local_db
   is
      jmsg : ThickUCL.UclTree;
   begin
      jmsg.insert ("type", "ERROR_NOLOCALDB");
      jmsg.start_object ("data");
      jmsg.close_object;
      pipe_event (ThickUCL.Emitter.emit_compact_ucl (jmsg, True));
      warnx ("Local package database nonexistent!");
   end emit_no_local_db;


   --------------------------
   --  emit_install_begin  --
   --------------------------
   procedure emit_install_begin (pkg : Pkgtypes.A_Package)
   is
      jmsg : ThickUCL.UclTree;
   begin
      jmsg.insert ("type", "INFO_INSTALL_BEGIN");
      jmsg.start_object ("data");
      jmsg.insert ("namebase", USS (pkg.namebase));
      jmsg.insert ("subpackage", USS (pkg.subpackage));
      jmsg.insert ("variant", USS (pkg.variant));
      jmsg.insert ("version", USS (pkg.version));
      jmsg.close_object;
      pipe_event (ThickUCL.Emitter.emit_compact_ucl (jmsg, True));
   end emit_install_begin;


   ------------------------
   --  emit_install_end  --
   ------------------------
   procedure emit_install_end (pkg : Pkgtypes.A_Package)
   is
      jmsg : ThickUCL.UclTree;
   begin
      jmsg.insert ("type", "INFO_INSTALL_FINISHED");
      jmsg.start_object ("data");
      jmsg.insert ("namebase", USS (pkg.namebase));
      jmsg.insert ("subpackage", USS (pkg.subpackage));
      jmsg.insert ("variant", USS (pkg.variant));
      jmsg.insert ("version", USS (pkg.version));
      jmsg.insert ("message", Pkgtypes.combined_messages (pkg, Pkgtypes.install));
      jmsg.close_object;
      pipe_event (ThickUCL.Emitter.emit_compact_ucl (jmsg, True));
   end emit_install_end;


   --------------------------
   --  emit_extract_begin  --
   --------------------------
   procedure emit_extract_begin (pkg : Pkgtypes.A_Package)
   is
      jmsg : ThickUCL.UclTree;
   begin
      jmsg.insert ("type", "INFO_EXTRACT_BEGIN");
      jmsg.start_object ("data");
      jmsg.insert ("namebase", USS (pkg.namebase));
      jmsg.insert ("subpackage", USS (pkg.subpackage));
      jmsg.insert ("variant", USS (pkg.variant));
      jmsg.insert ("version", USS (pkg.version));
      jmsg.close_object;
      pipe_event (ThickUCL.Emitter.emit_compact_ucl (jmsg, True));
   end emit_extract_begin;


   ------------------------
   --  emit_extract_end  --
   ------------------------
   procedure emit_extract_end (pkg : Pkgtypes.A_Package)
   is
      jmsg : ThickUCL.UclTree;
   begin
      jmsg.insert ("type", "INFO_EXTRACT_FINISHED");
      jmsg.start_object ("data");
      jmsg.insert ("namebase", USS (pkg.namebase));
      jmsg.insert ("subpackage", USS (pkg.subpackage));
      jmsg.insert ("variant", USS (pkg.variant));
      jmsg.insert ("version", USS (pkg.version));
      jmsg.close_object;
      pipe_event (ThickUCL.Emitter.emit_compact_ucl (jmsg, True));
   end emit_extract_end;


   -------------------------
   --  emit_remove_begin  --
   -------------------------
   procedure emit_remove_begin  (pkg : Pkgtypes.A_Package)
   is
      jmsg : ThickUCL.UclTree;
   begin
      jmsg.insert ("type", "INFO_REMOVE_BEGIN");
      jmsg.start_object ("data");
      jmsg.insert ("namebase", USS (pkg.namebase));
      jmsg.insert ("subpackage", USS (pkg.subpackage));
      jmsg.insert ("variant", USS (pkg.variant));
      jmsg.insert ("version", USS (pkg.version));
      jmsg.close_object;
      pipe_event (ThickUCL.Emitter.emit_compact_ucl (jmsg, True));
   end emit_remove_begin;


   -----------------------
   --  emit_remove_end  --
   -----------------------
   procedure emit_remove_end (pkg : Pkgtypes.A_Package)
   is
      jmsg : ThickUCL.UclTree;
   begin
      jmsg.insert ("type", "INFO_REMOVE_FINISHED");
      jmsg.start_object ("data");
      jmsg.insert ("namebase", USS (pkg.namebase));
      jmsg.insert ("subpackage", USS (pkg.subpackage));
      jmsg.insert ("variant", USS (pkg.variant));
      jmsg.insert ("version", USS (pkg.version));
      jmsg.insert ("message", Pkgtypes.combined_messages (pkg, Pkgtypes.deinstall));
      jmsg.close_object;
      pipe_event (ThickUCL.Emitter.emit_compact_ucl (jmsg, True));
   end emit_remove_end;


   --------------------------
   --  emit_upgrade_begin  --
   --------------------------
   procedure emit_upgrade_begin (pkg : Pkgtypes.A_Package; newpkg : Pkgtypes.A_Package)
   is
      jmsg : ThickUCL.UclTree;
   begin
      jmsg.insert ("type", "INFO_UPGRADE_BEGIN");
      jmsg.start_object ("data");
      jmsg.insert ("namebase", USS (pkg.namebase));
      jmsg.insert ("subpackage", USS (pkg.subpackage));
      jmsg.insert ("variant", USS (pkg.variant));
      jmsg.insert ("version", USS (pkg.version));
      jmsg.insert ("newversion", USS (newpkg.version));
      jmsg.close_object;
      pipe_event (ThickUCL.Emitter.emit_compact_ucl (jmsg, True));
   end emit_upgrade_begin;


   ------------------------
   --  emit_upgrade_end  --
   ------------------------
   procedure emit_upgrade_end (pkg : Pkgtypes.A_Package; newpkg : Pkgtypes.A_Package)
   is
      jmsg : ThickUCL.UclTree;
   begin
      jmsg.insert ("type", "INFO_UPGRADE_FINISHED");
      jmsg.start_object ("data");
      jmsg.insert ("namebase", USS (pkg.namebase));
      jmsg.insert ("subpackage", USS (pkg.subpackage));
      jmsg.insert ("variant", USS (pkg.variant));
      jmsg.insert ("version", USS (pkg.version));
      jmsg.insert ("newversion", USS (newpkg.version));
      jmsg.insert ("message", Pkgtypes.combined_messages (newpkg, Pkgtypes.upgrade));
      jmsg.close_object;
      pipe_event (ThickUCL.Emitter.emit_compact_ucl (jmsg, True));
   end emit_upgrade_end;


   ------------------------
   --  emit_fetch_begin  --
   ------------------------
   procedure emit_fetch_begin (url : String; rvnsize : Pkgtypes.Package_Size)
   is
      jmsg : ThickUCL.UclTree;
   begin
      jmsg.insert ("type", "INFO_FETCH_BEGIN");
      jmsg.start_object ("data");
      jmsg.insert ("url", url);
      jmsg.insert ("size", Ucl.ucl_integer (rvnsize));
      jmsg.close_object;
      pipe_event (ThickUCL.Emitter.emit_compact_ucl (jmsg, True));
   end emit_fetch_begin;


   ---------------------------
   --  emit_fetch_finished  --
   ---------------------------
   procedure emit_fetch_finished (url : String; rvnsize : Pkgtypes.Package_Size)
   is
      jmsg : ThickUCL.UclTree;
   begin
      jmsg.insert ("type", "INFO_FETCH_FINISHED");
      jmsg.start_object ("data");
      jmsg.insert ("url", url);
      jmsg.insert ("size", Ucl.ucl_integer (rvnsize));
      jmsg.close_object;
      pipe_event (ThickUCL.Emitter.emit_compact_ucl (jmsg, True));
   end emit_fetch_finished;


end Raven.Event;
