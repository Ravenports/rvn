--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Strings; use Raven.Strings;

package body Raven.Context is


   ---------------------------------
   --  reveal_cache_directory_fd  --
   ---------------------------------
   function reveal_cache_directory_fd return Unix.File_Descriptor is
   begin
      if not Unix.file_connected (context.cachedir_fd) then
         context.cachedir_fd := Unix.open_file (USS (context.cachedir), dirflags);
      end if;
      return context.cachedir_fd;
   end reveal_cache_directory_fd;


   ------------------------------
   --  reveal_db_directory_fd  --
   ------------------------------
   function reveal_db_directory_fd return Unix.File_Descriptor is
   begin
      if not Unix.file_connected (context.dbdir_fd) then
         context.dbdir_fd := Unix.open_file (USS (context.dbdir), dirflags);
      end if;
      return context.dbdir_fd;
   end reveal_db_directory_fd;


   --------------------------------
   --  reveal_root_directory_fd  --
   --------------------------------
   function reveal_root_directory_fd return Unix.File_Descriptor is
   begin
      if not Unix.file_connected (context.rootdir_fd) then
         declare
            flags : Unix.T_Open_Flags;
         begin
            flags.DIRECTORY := True;
            flags.RDONLY    := True;
            flags.CLOEXEC   := True;
            context.rootdir_fd := Unix.open_file ("/", flags);
            --  Caller has to check for success.
         end;
      end if;
      return context.rootdir_fd;
   end reveal_root_directory_fd;


   ------------------------------------
   --  register_event_pipe_via_file  --
   ------------------------------------
   function register_event_pipe_via_file (pipe_name : String) return Boolean
   is
      sock_flags  : Unix.T_Open_Flags;
   begin
      if Unix.file_connected (context.eventpipe) then
         return True;
      else
         sock_flags.WRONLY := True;
         sock_flags.NON_BLOCK := True;
         context.eventpipe := Unix.open_file (pipe_name, sock_flags);
         return Unix.file_connected (context.eventpipe);
      end if;
   end register_event_pipe_via_file;


   --------------------------------------
   --  register_event_pipe_via_socket  --
   --------------------------------------
   function register_event_pipe_via_socket (pipe_name : String) return Unix.Unix_Socket_Result
   is
   begin
      if Unix.file_connected (context.eventpipe) then
         return Unix.connected;
      end if;

      --  Caller has to handle failure
      return Unix.connect_unix_socket (pipe_name, context.eventpipe);
   end register_event_pipe_via_socket;


   -------------------------
   --  reveal_event_pipe  --
   -------------------------
   function reveal_event_pipe return Unix.File_Descriptor is
   begin
      return context.eventpipe;
   end reveal_event_pipe;


   -----------------------------
   --  reveal_developer_mode  --
   -----------------------------
   function reveal_developer_mode return Boolean is
   begin
      return context.developer_mode;
   end reveal_developer_mode;


   ------------------------
   --  close_event_pipe  --
   ------------------------
   procedure close_event_pipe is
   begin
      Unix.close_file_blind (context.eventpipe);
   end close_event_pipe;


   -------------------------------
   --  close_root_directory_fd  --
   -------------------------------
   procedure close_root_directory_fd is
   begin
      Unix.close_file_blind (context.rootdir_fd);
   end close_root_directory_fd;


   --------------------------------
   --  close_cache_directory_fd  --
   --------------------------------
   procedure close_cache_directory_fd is
   begin
      Unix.close_file_blind (Context.cachedir_fd);
   end close_cache_directory_fd;


   -----------------------------
   --  close_db_directory_fd  --
   -----------------------------
   procedure close_db_directory_fd is
   begin
      Unix.close_file_blind (context.dbdir_fd);
   end close_db_directory_fd;


   --------------------------
   --  reveal_debug_level  --
   --------------------------
   function reveal_debug_level return A_Debug_Level is
   begin
      return Context.debug_level;
   end reveal_debug_level;


   -----------------------------
   --  reveal_case_sensitive  --
   -----------------------------
   function reveal_case_sensitive return Boolean is
   begin
      return Context.case_sensitive;
   end reveal_case_sensitive;


   ----------------------------
   --  register_debug_level  --
   ----------------------------
   procedure register_debug_level (level : A_Debug_Level) is
   begin
      Context.debug_level := level;
   end register_debug_level;


   -------------------------
   --  register_dev_mode  --
   -------------------------
   procedure register_dev_mode (mode_on : Boolean) is
   begin
      Context.developer_mode := mode_on;
   end register_dev_mode;


   --------------------------------
   --  register_cache_directory  --
   --------------------------------
   procedure register_cache_directory (dir : String) is
   begin
      context.cachedir := SUS (dir);
   end register_cache_directory;


   ------------------------------
   --  reveal_cache_directory  --
   ------------------------------
   function reveal_cache_directory return String is
   begin
      return USS (context.cachedir);
   end reveal_cache_directory;


   ---------------------------
   --  reveal_db_directory  --
   ---------------------------
   function reveal_db_directory return String is
   begin
      return USS (context.dbdir);
   end reveal_db_directory;


   -----------------------------
   --  register_db_directory  --
   -----------------------------
   procedure register_db_directory (dir: String) is
   begin
      context.dbdir := SUS (dir);
   end register_db_directory;


   ---------------------------------
   --  register_case_sensitivity  --
   ---------------------------------
   procedure register_case_sensitivity (sensitive : Boolean) is
   begin
      context.case_sensitive := sensitive;
   end register_case_sensitivity;


end Raven.Context;
