with Ada.Real_Time;
with Archive.Unix;

package body Raven.Miscellaneous is

   package RT  renames Ada.Real_Time;


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


end Raven.Miscellaneous;
