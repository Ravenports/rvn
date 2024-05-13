--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Unix;

package Raven.Context is

   function reveal_db_directory_fd return Unix.File_Descriptor;
   function reveal_root_directory_fd return Unix.File_Descriptor;
   function reveal_cache_directory_fd return Unix.File_Descriptor;
   function reveal_event_pipe return Unix.File_Descriptor;
   function reveal_debug_level return A_Debug_Level;
   function reveal_developer_mode return Boolean;
   function reveal_case_sensitive return Boolean;
   function reveal_cache_directory return String;
   function reveal_db_directory return String;
   function reveal_protocol_restriction return IP_support;

   procedure close_event_pipe;
   procedure close_root_directory_fd;
   procedure close_cache_directory_fd;
   procedure close_db_directory_fd;

   procedure register_debug_level (level : A_Debug_Level);
   procedure register_dev_mode (mode_on : Boolean);
   procedure register_cache_directory (dir : String);
   procedure register_db_directory (dir: String);
   procedure register_case_sensitivity (sensitive : Boolean);
   procedure register_protocol_restriction (setting : IP_support);
   function register_event_pipe_via_file (pipe_name : String) return Boolean;
   function register_event_pipe_via_socket (pipe_name : String) return Unix.Unix_Socket_Result;

private

   type A_context is
      record
         eventpipe      : Unix.File_Descriptor := Unix.not_connected;
         debug_level    : A_Debug_Level := A_Debug_Level'First;
         developer_mode : Boolean := False;
         case_sensitive : Boolean := False;
         cachedir       : Text;
         dbdir          : Text;
         rootdir_fd     : Unix.File_Descriptor := Unix.not_connected;
         cachedir_fd    : Unix.File_Descriptor := Unix.not_connected;
         dbdir_fd       : Unix.File_Descriptor := Unix.not_connected;
         internet_proto : IP_support := no_restriction;
      end record;

   context : A_context;

   dirflags  : constant Unix.T_Open_Flags := (DIRECTORY => True,
                                              CLOEXEC => True,
                                              others => False);

end Raven.Context;
