--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Interfaces.C.Strings;
with Raven.Unix;


package Raven.VFS is

   package IC  renames Interfaces.C;
   package ICS renames Interfaces.C.Strings;

   function dbdir_open (path : ICS.chars_ptr; flags : IC.int; mode : IC.int) return IC.int;
   pragma Export (C, dbdir_open);

   function dbdir_access (path : ICS.chars_ptr; mode : IC.int) return IC.int;
   pragma Export (C, dbdir_access);

   function dbdir_unlink (path : ICS.chars_ptr) return IC.int;
   pragma Export (C, dbdir_unlink);

   function dbdir_stat (path : ICS.chars_ptr; sb : Unix.struct_stat_Access) return IC.int;
   pragma Export (C, dbdir_stat);

   function dbdir_lstat (path : ICS.chars_ptr; sb : Unix.struct_stat_Access) return IC.int;
   pragma Export (C, dbdir_lstat);

   function dbdir_mkdir (path : ICS.chars_ptr; mode : IC.int) return IC.int;
   pragma Export (C, dbdir_mkdir);

end Raven.VFS;
