--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Calendar.Conversions;

with Raven.Strings;
with Raven.Version;
with Raven.Cmd.Unset;
with Raven.Depends;
with Archive.Unix;
with SQLite;
with Blake_3;

use Raven.Strings;

package body Raven.Database.CustomCmds is

   package RCU renames Raven.Cmd.Unset;

   --------------------------------------------------------------------
   --  rdb_vercmp
   --------------------------------------------------------------------
   procedure rdb_vercmp
     (context : not null SQLite.db3_context;
      numargs : IC.int;
      argsval : not null access SQLite.db3_value)
   is
      use type IC.int;

      argv : array (1 .. 3) of SQLite.db3_value;

      for argv'Address use argsval.all'Address;
      pragma Import (Ada, argv);
   begin
      if numargs /= 3 then
         SQLite.set_sqlite_error (context, "Invalid usage of vercmp(): needs 3 arguments", -1);
         return;
      end if;

      declare
         op_str : constant String := SQLite.get_value (argv (1));
         arg1   : constant String := SQLite.get_value (argv (2));
         arg2   : constant String := SQLite.get_value (argv (3));
         op     : Depends.Dep_Version_Operator;
         cmp    : Version.Cmp_Result;
         ret    : Boolean;
         result : Integer := 0;
      begin
         if op_str = "" or else
           arg1 = "" or else
           arg2 = ""
         then
            SQLite.set_sqlite_error (context, "Invalid usage of vercmp(): empty arguments", -1);
            return;
         end if;

         op := Depends.string_to_operator (op_str);
         cmp := Version.pkg_version_cmp (arg1, arg2);

         case op is
            when Depends.VERSION_EQ  => ret := (cmp = 0);
            when Depends.VERSION_GE  => ret := (cmp >= 0);
            when Depends.VERSION_LE  => ret := (cmp <= 0);
            when Depends.VERSION_GT  => ret := (cmp > 0);
            when Depends.VERSION_LT  => ret := (cmp < 0);
            when Depends.VERSION_NOT => ret := (cmp /= 0);
            when Depends.VERSION_ANY => ret := True;
         end case;

         if ret then
            result := 1;
         end if;
         SQLite.set_integer_result (context, result);
      end;
   end rdb_vercmp;


   --------------------------------------------------------------------
   --  rdb_regex
   --------------------------------------------------------------------
   procedure rdb_regex
     (context : not null SQLite.db3_context;
      numargs : IC.int;
      argsval : not null access SQLite.db3_value)
   is
      argv  : array (1 .. 2) of SQLite.db3_value;
      flags : Natural := SQLite.REX_NOCASE;

      for argv'Address use argsval.all'Address;
      pragma Import (Ada, argv);
   begin
      if Natural (numargs) /= 2 then
         SQLite.set_sqlite_error (context, "Invalid usage of regex(): needs 2 arguments", -1);
         return;
      end if;

      declare
         regex     : constant String := SQLite.get_value (argv (1));
         str       : constant String := SQLite.get_value (argv (2));
         re_Access : SQLite.reg_expression;

         use type SQLite.reg_expression;
      begin
         if regex = "" or else
           str = ""
         then
            SQLite.set_sqlite_error (context, "Invalid usage of regex(): empty arguments", -1);
            return;
         end if;

         re_Access := SQLite.sqlite3_get_auxdata_as_regex (context, 0);

         if re_Access = null then
            if Database.rdb_case_sensitive then
               flags := SQLite.REX_CASE;
            end if;
            re_Access := re'Access;
            if not SQLite.regex_compile (re_Access, regex, flags) then
               SQLite.set_sqlite_error (context, "Invalid regex", -1);
               return;
            end if;

            SQLite.sqlite3_set_auxdata_as_regex (context  => context,
                                                 N        => 0,
                                                 data     => re_Access,
                                                 callback => rdb_regex_delete'Access);
         end if;
         if SQLite.regex_match_found (re_Access, str) then
            SQLite.set_integer_result (context, 1);
         else
            SQLite.set_integer_result (context, 0);
         end if;
      end;
   end rdb_regex;


   --------------------------------------------------------------------
   --  rdb_now
   --------------------------------------------------------------------
   procedure rdb_now
     (context : not null SQLite.db3_context ;
      numargs : IC.int;
      argsval : not null access SQLite.db3_value)
   is
      Now    : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      epoch  : constant IC.long := Ada.Calendar.Conversions.To_Unix_Time (Now);
   begin
      if Natural (numargs) /= 0 then
         SQLite.set_sqlite_error (context, "Invalid usage of now(): no arguments expected", -1);
         return;
      end if;

      SQLite.set_integer64_result (context, SQLite.sql_int64 (epoch));
   end rdb_now;


   --------------------------------------------------------------------
   --  rdb_regex_delete
   --------------------------------------------------------------------
   procedure rdb_regex_delete (regex_ptr : not null SQLite.reg_expression) is
   begin
      SQLite.free_expression (regex_ptr);
   end rdb_regex_delete;


   --------------------------------------------------------------------
   --  sqlite_file_exists
   --------------------------------------------------------------------
   procedure rdb_file_exists
     (context : not null SQLite.db3_context;
      numargs : IC.int;
      argsval : not null access SQLite.db3_value)
   is
      db : constant SQLite.db3 := SQLite.get_db_handle (context);
      db_filename : constant String := SQLite.get_db_filename (db, SQLite.primary_db_identity);
      argv : array (1 .. 2) of SQLite.db3_value;

      for argv'Address use argsval.all'Address;
      pragma Import (Ada, argv);
   begin
      if Natural (numargs) /= 2 then
         SQLite.set_sqlite_error (context, "Invalid usage of file_exists(): needs 2 arguments", -1);
         return;
      end if;

      declare
         dpath : constant String := head (db_filename, "/");  -- db_filename is absolete path
         arg1  : constant String := SQLite.get_value (argv (1));
         b3sum : constant String := SQLite.get_value (argv (2));
         fpath : constant String := dpath & "/" & arg1;
         feats : Archive.Unix.File_Characteristics;
      begin
         feats := Archive.Unix.get_charactistics (fpath);
         case feats.ftype is
            when Archive.unsupported | Archive.directory => null;
            when Archive.symlink | Archive.fifo => null;
            when Archive.regular | Archive.hardlink =>
               if Blake_3.hex (Blake_3.file_digest (fpath)) = b3sum then
                  SQLite.set_integer_result (context, 1);
                  return;
               end if;
         end case;
         SQLite.set_integer_result (context, 0);
      end;
   end rdb_file_exists;


   --------------------------------------------------------------------
   --  sqlcmd_init
   --------------------------------------------------------------------
   function sqlcmd_init
     (db       : not null SQLite.db3;
      pzErrMsg : access ICS.chars_ptr;
      pThunk   : SQLite.db3_routine) return IC.int is
   begin
      SQLite.create_function (db, "now",    0, rdb_now'Access);
      SQLite.create_function (db, "regexp", 2, rdb_regex'Access);
      SQLite.create_function (db, "vercmp", 3, rdb_vercmp'Access);
      return IC.int (SQLite.SQL_OK);
   end sqlcmd_init;


   --------------------------------------------------------------------
   --  define_file_exists
   --------------------------------------------------------------------
   procedure define_file_exists (db : not null SQLite.db3) is
   begin
      SQLite.create_function (db, "file_exists", 2, rdb_file_exists'Access);
   end define_file_exists;


   --------------------------------------------------------------------
   --  define_six_functions
   --------------------------------------------------------------------
   procedure define_six_functions (db : not null SQLite.db3)
   is
      unused_result : IC.int;
      pragma Unreferenced (unused_result);
   begin
      unused_result := sqlcmd_init (db, null, null);
   end define_six_functions;


end Raven.Database.CustomCmds;
