--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Containers;

package Raven.Miscellaneous is

   --  Returns temporary filename confirmed to not already being used.
   --  tmp & ".rvn_<midname>." & extension;
   function get_temporary_filename (midname : String) return String;

   --  Given the pattern "<path/to/parent-dir>/<basename>.<extension>" return basename
   function archive_basename (path_to_archive : String) return String;

   --  Used for mapped containers
   function map_hash (key : Text) return Ada.Containers.Hash_Type;

private

   function random_extension return String;
   function tmp return String;

end Raven.Miscellaneous;
