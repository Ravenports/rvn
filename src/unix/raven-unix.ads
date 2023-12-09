--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Interfaces.C.Strings;

package Raven.Unix is

   package IC renames Interfaces.C;

   bad_stat   : exception;
   bad_execvp : exception;

   type Unix_Pipe is (named_pipe, unix_socket, something_else);
   type Unix_Socket_Result is (connected, failed_creation, failed_population, failed_connection);

   type Process_ID is new Integer;
   type File_Descriptor is new Integer;
   type uid_t is new Integer;
   not_connected  : constant File_Descriptor := -1;
   stdin_file_fd  : constant File_Descriptor := 0;
   stdout_file_fd : constant File_Descriptor := 1;
   stderr_file_fd : constant File_Descriptor := 2;

      --  Set both RDONLY and WRONLY to get RDRW flags
   type T_Open_Flags is
      record
         RDONLY    : Boolean := False;
         WRONLY    : Boolean := False;
         NON_BLOCK : Boolean := False;
         DIRECTORY : Boolean := False;
         CLOEXEC   : Boolean := False;
         CREAT     : Boolean := False;
         TRUNC     : Boolean := False;
      end record;

   --  Use libc's open function to retrieve file descriptor
   function open_file (filename : String; flags : T_Open_Flags) return File_Descriptor;

   --  Closes file without checking if the attempt was successful or not
   procedure close_file_blind (fd : File_Descriptor);

   --  Connect to Unix-style socket
   function connect_unix_socket
     (filename : String;
      fd       : out File_Descriptor) return Unix_Socket_Result;

   --  Return True if fd /= -1
   function file_connected (fd : File_Descriptor) return Boolean;

   --  Send log down file descriptor of event pipe
   procedure push_to_event_pipe (fd : File_Descriptor; message : String);

   --  Get Process ID
   function getpid return Process_ID;
   pragma Import (C, getpid, "getpid");

   --  call C function to determine if FIFO or Socket or something else
   function IPC_mechanism (filename : String) return Unix_Pipe;

   --  Last seen error number by C function
   function errno return Integer;

   --  strerror from libc
   function strerror (errno : Integer) return String;

private

   last_errno : Integer;

   function C_Close (fd : IC.int) return IC.int;
   pragma Import (C, C_Close, "close");

   function C_Connect (path  : IC.Strings.chars_ptr; newfd : out IC.int) return IC.int;
   pragma Import (C, C_Connect, "connect_socket");

   function C_Errno return IC.int;
   pragma Import (C, C_Errno, "get_errno");

   function C_Open
     (path      : IC.Strings.chars_ptr;
      rdonly    : IC.int;
      wronly    : IC.int;
      nonblock  : IC.int;
      directory : IC.int;
      cloexec   : IC.int;
      creat     : IC.int;
      trunc     : IC.int) return IC.int;
   pragma Import (C, C_Open, "rvn_try_open");

   function C_dprint (fd : IC.int; msg : IC.Strings.chars_ptr) return IC.int;
   pragma Import (C, C_dprint, "dprint");

   function C_IPC (path : IC.Strings.chars_ptr) return IC.int;
   pragma Import (C, C_IPC, "detect_IPC");

   function C_Strerror (Errnum : IC.int) return IC.Strings.chars_ptr;
   pragma Import (C, C_Strerror, "strerror");

end Raven.Unix;
