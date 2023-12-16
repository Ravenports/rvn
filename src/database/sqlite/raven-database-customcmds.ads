--  SPDX-License-Identifier: ISC
--  Reference: /License.txt


with Interfaces.C.Strings;
with SQLite;

package Raven.Database.CustomCmds is

   package IC  renames Interfaces.C;
   package ICS renames Interfaces.C.Strings;

   --  Note: When building the internal SQLite:
   --  1) Remove internal check with sed: /verify_uninitialized/d from shell.c
   --  2) sed 's|SQLITE_CDECL main|SQLITE_CDECL sqlite3_shell|' on shell.c (obsolete
   --     with recent builds)

   --  Defines custom sql functions for sqlite
   function sqlcmd_init
     (db       : not null SQLite.db3;
      pzErrMsg : access ICS.chars_ptr;
      pThunk   : SQLite.db3_routine) return IC.int;
   pragma Export (C, sqlcmd_init);

   --  Defines custom function file_exists in database
   procedure define_file_exists (db : not null SQLite.db3);

   --  wrapper around sqlcmd_init()
   procedure define_six_functions (db : not null SQLite.db3);

private

   --  regex object must be global because it will be referenced after rdb_regex ends
   re : aliased SQLite.regex_storage;

   --  select now();
   --  returns 1567046088
   --
   --  takes no arguments
   --  Function returns unix epoch as defined by system clock
   procedure rdb_now
     (context : not null SQLite.db3_context;
      numargs : IC.int;
      argsval : not null access SQLite.db3_value);
   pragma Convention (C, rdb_now);

   --  sqlite> select regexp ("[0-9]e", "abc3ef");
   --  returns 1
   --  sqlite> select regexp ("[0-9]f", "abc3ef");
   --  returns 0
   --
   --  arg1 = regular expression string
   --  arg2 = input string
   --  returns 0 (false) or 1 (true) if a match is found
   --  Function returns True if given string has a match against given regular expression
   procedure rdb_regex
     (context : not null SQLite.db3_context;
      numargs : IC.int;
      argsval : not null access SQLite.db3_value);
   pragma Convention (C, rdb_regex);

   --  select vercmp("<=", "joe-1.0", "joe-1.1");
   --  returns 1.
   --
   --  arg1 = operator string ("==", "!=", "<", ">", "<=", ">=", anything else)
   --  arg2 = package 1 name
   --  arg3 = package 2 name
   --  Function compares package 2 name against package 1 name and returns 0 or 1.
   procedure rdb_vercmp
     (context : not null SQLite.db3_context;
      numargs : IC.int;
      argsval : not null access SQLite.db3_value);
   pragma Convention (C, rdb_vercmp);

   --  callback for pkgdb_regex
   procedure rdb_regex_delete (regex_ptr : not null SQLite.reg_expression);
   pragma Convention (C, rdb_regex_delete);

   --  2 arguments: filename and checksum.  Returns true if file exists and checksum matches
   procedure rdb_file_exists
     (context : not null SQLite.db3_context;
      numargs : IC.int;
      argsval : not null access SQLite.db3_value);
   pragma Convention (C, rdb_file_exists);

end Raven.Database.CustomCmds;
