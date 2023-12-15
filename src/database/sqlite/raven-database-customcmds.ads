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

   --  select myarch();
   --  returns "DragonFly:5.8:x86_64"
   --  select myarch(null);
   --  returns "DragonFly:5.8:x86_64"
   --  select myarch("OpenBSD:6.4:amd64");
   --  returns "OpenBSD:6.4:amd64";
   --
   --  arg1 optional.  Will override ABI if present
   --  Function returns configured ABI unless overridden in arguments (then it returns that)
   procedure rdb_myarch
     (context : not null SQLite.db3_context;
      numargs : IC.int;
      argsval : not null access SQLite.db3_value);
   pragma Convention (C, rdb_myarch);

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

   --  select split_version ("nsv", "joe-single-standard-1.0");
   --  returns "joe-single-standard"
   --  select split_version ("version", "joe-single-standard-1.0_1,2");
   --  returns "1.0_1,2"
   --
   --  arg1 = "nsv" or "version"
   --  arg2 = package name
   --  function returns name part or version part of package name
   procedure rdb_split_version
     (context : not null SQLite.db3_context;
      numargs : IC.int;
      argsval : not null access SQLite.db3_value);
   pragma Convention (C, rdb_split_version);

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

   --  Where split routine does the actual work (allows custom split words, delimiter, etc)
   procedure rdb_split_common
     (context : not null SQLite.db3_context;
      numargs : IC.int;
      argsval : not null access SQLite.db3_value;
      delim   : Character;
      first   : String;
      second  : String);

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
