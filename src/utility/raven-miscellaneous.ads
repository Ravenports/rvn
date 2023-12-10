package Raven.Miscellaneous is

   --  Returns temporary filename confirmed to not already being used.
   --  tmp & ".rvn_<midname>." & extension;
   function get_temporary_filename (midname : String) return String;

private

   function random_extension return String;
   function tmp return String;

end Raven.Miscellaneous;
