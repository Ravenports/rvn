--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

package Raven.Miscellaneous is

   --  Returns temporary filename confirmed to not already being used.
   --  tmp & ".rvn_<midname>." & extension;
   function get_temporary_filename (midname : String) return String;

   --  Given the pattern "<path/to/parent-dir>/<basename>.<extension>" return basename
   function archive_basename (path_to_archive : String) return String;

   --  This is operating-system specific (normally /bin/sh though)
   function get_interpreter return String;

private

   function random_extension return String;
   function tmp return String;

end Raven.Miscellaneous;
