--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt


with Ada.Characters.Latin_1;

package body Raven.Unix is

   package LAT renames Ada.Characters.Latin_1;

   ------------------------
   --  close_file_blind  --
   ------------------------
   procedure close_file_blind (fd : File_Descriptor)
   is
      result : IC.int;
      pragma Unreferenced (result);
   begin
      if fd = not_connected then
         return;
      end if;
      result := C_Close (IC.int (fd));
   end close_file_blind;


   ----------------------
   --  file_connected  --
   ----------------------
   function file_connected (fd : File_Descriptor) return Boolean is
   begin
      return fd /= not_connected;
   end file_connected;


   ---------------------------
   --  connect_unix_socket  --
   ---------------------------
   function connect_unix_socket (filename : String; fd : out File_Descriptor)
                                 return Unix_Socket_Result
   is
      result : IC.int;
      new_fd : IC.int;
      name   : IC.Strings.chars_ptr;
   begin
      name := IC.Strings.New_String (filename);
      result := C_Connect (path  => name, newfd => new_fd);
      IC.Strings.Free (name);

      case result is
         when 1 =>
            fd := File_Descriptor (new_fd);
         when others =>
            fd := not_connected;
            last_errno := Integer (C_Errno);
      end case;

      case result is
         when      1 => return connected;
         when      2 => return failed_creation;
         when      3 => return failed_population;
         when others => return failed_connection;
      end case;
   end connect_unix_socket;


   --------------------
   --  open_file #1  --
   --------------------
   function open_file (filename : String; flags : T_Open_Flags) return File_Descriptor
   is
      result         : File_Descriptor;
      flag_wronly    : IC.int := IC.int (0);
      flag_nonblock  : IC.int := IC.int (0);
      flag_rdonly    : IC.int := IC.int (0);
      flag_directory : IC.int := IC.int (0);
      flag_cloexec   : IC.int := IC.int (0);
      flag_creat     : IC.int := IC.int (0);
      flag_trunc     : IC.int := IC.int (0);
      name           : IC.Strings.chars_ptr;
   begin
      if flags.WRONLY then
         flag_wronly := IC.int (1);
      end if;
      if flags.NON_BLOCK then
         flag_nonblock := IC.int (1);
      end if;
      if flags.RDONLY then
         flag_rdonly := IC.int (1);
      end if;
      if flags.DIRECTORY then
         flag_directory := IC.int (1);
      end if;
      if flags.CLOEXEC then
         flag_cloexec := IC.int (1);
      end if;
      if flags.CREAT then
         flag_creat := IC.int (1);
      end if;
      if flags.TRUNC then
         flag_trunc := IC.int (1);
      end if;

      name := IC.Strings.New_String (filename);
      result := File_Descriptor (C_Open (path      => name,
                                         rdonly    => flag_rdonly,
                                         wronly    => flag_wronly,
                                         nonblock  => flag_nonblock,
                                         directory => flag_directory,
                                         cloexec   => flag_cloexec,
                                         creat     => flag_creat,
                                         trunc     => flag_trunc));
      IC.Strings.Free (name);
      if not file_connected (result) then
         last_errno := Integer (C_Errno);
      end if;
      return result;
   end open_file;


   --------------------------
   --  push_to_event_pipe  --
   --------------------------
   procedure push_to_event_pipe (fd : File_Descriptor; message : String)
   is
      msg    : IC.Strings.chars_ptr;
      result : IC.int;
   begin
      msg := IC.Strings.New_String (message & LAT.LF);
      result := C_dprint (IC.int (fd), msg);  -- returns #chars printed
      IC.Strings.Free (msg);
   end push_to_event_pipe;

end Raven.Unix;
