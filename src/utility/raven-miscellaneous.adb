--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Real_Time;
with Archive.Unix;
with Raven.Strings; use Raven.Strings;


package body Raven.Miscellaneous is

   package RT  renames Ada.Real_Time;


   ------------------------
   --  archive_basename  --
   ------------------------
   function archive_basename (path_to_archive : String) return String
   is
      filename : String := tail (path_to_archive, "/");
   begin
      return head (filename, ".");
   end archive_basename;


   ------------------------------
   --  get_temporary_filename  --
   ------------------------------
   function get_temporary_filename (midname : String) return String
   is
      tmpdir : constant String := tmp;
   begin
      loop
         declare
            extension  : constant String := random_extension;
            candidate  : constant String := tmpdir & ".rvn_" & midname & "." & extension;
         begin
            if not Archive.Unix.file_exists (candidate) then
               return candidate;
            end if;
         end;
      end loop;
   end get_temporary_filename;


   ------------------------
   --  random_extension  --
   ------------------------
   function random_extension return String
   is
      right_now : constant RT.Time := RT.Clock;
      seconds   : RT.Seconds_Count;
      nanospan  : RT.Time_Span;
      nduration : Duration;
   begin
      RT.Split (right_now, seconds, nanospan);
      nduration := RT.To_Duration (nanospan);
      declare
         durstr : constant String := nduration'Img;
      begin
         --  durstr in format " 0.xxxxxxxxx" (leading space)
         return durstr (durstr'First + 3 .. durstr'Last);
      end;
   end random_extension;


   -----------
   --  tmp  --
   -----------
   function tmp return String
   is
      root_tmp : constant String := "/tmp";
      attributes : Archive.Unix.File_Characteristics;
   begin
      attributes := Archive.Unix.get_charactistics (root_tmp);
      case attributes.ftype is
         when Archive.directory => return root_tmp & "/";
         when others => return "";
      end case;
   end tmp;


   -----------------------
   --  get_interpreter  --
   -----------------------
   function get_interpreter return String
   is
      standard_interpreter : constant String := "/bin/sh";
   begin
      case platform is
         when generic_unix |
              dragonfly    |
              freebsd      |
              openbsd      |
              netbsd       => return standard_interpreter;
         when linux        => return standard_interpreter;
         when omnios       |
              solaris      => return "/usr/xpg4/bin/sh";
      end case;
   end get_interpreter;

end Raven.Miscellaneous;