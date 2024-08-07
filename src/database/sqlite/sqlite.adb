--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Unchecked_Conversion;
with System;

package body SQLite is

   package IC  renames Interfaces.C;
   package ICS renames Interfaces.C.Strings;

   --------------------------------------------------------------------
   --  initialize_sqlite
   --------------------------------------------------------------------
   function initialize_sqlite return Boolean
   is
      result : IC.int;

      use type IC.int;
   begin
      result := sqlite_h.sqlite3_initialize;
      return (result = sqlite_h.SQLITE_OK);
   end initialize_sqlite;


   --------------------------------------------------------------------
   --  open_sqlite_database_readonly
   --------------------------------------------------------------------
   function open_sqlite_database_readonly
     (path : String;
      ppDB : not null access db3)
      return Boolean
   is
      c_path  : ICS.chars_ptr;
      result  : IC.int;

      use type IC.int;
   begin
      c_path := ICS.New_String (path);
      result := sqlite_h.sqlite3_open_v2 (File_Name => c_path,
                                          Handle    => ppDB,
                                          flags     => sqlite_h.SQLITE_OPEN_READONLY,
                                          zVfs      => ICS.Null_Ptr);
      ICS.Free (c_path);
      return (result = sqlite_h.SQLITE_OK);
   end open_sqlite_database_readonly;


   --------------------------------------------------------------------
   --  open_sqlite_database_readwrite
   --------------------------------------------------------------------
   function open_sqlite_database_readwrite
     (path : String;
      ppDB : not null access db3)
      return Boolean
   is
      c_path  : ICS.chars_ptr;
      result  : IC.int;

      use type IC.int;
   begin
      c_path := ICS.New_String (path);
      result := sqlite_h.sqlite3_open (File_Name => c_path,
                                       Handle    => ppDB);
      ICS.Free (c_path);
      return (result = sqlite_h.SQLITE_OK);
   end open_sqlite_database_readwrite;


   --------------------------------------------------------------------
   --  prepare_sql
   --------------------------------------------------------------------
   function prepare_sql
     (pDB    : not null db3;
      sql    : String;
      stmt   : out thick_stmt) return Boolean
   is
      use type IC.int;

      c_sql     : ICS.chars_ptr;
      result    : IC.int;
      unlimited : constant IC.int := IC.int (-1);
      pzTail    : aliased ICS.chars_ptr := ICS.Null_Ptr;
   begin
      c_sql := ICS.New_String (sql);
      result := sqlite_h.sqlite3_prepare_v2 (db     => pDB,
                                             zSql   => c_sql,
                                             nByte  => unlimited,
                                             ppStmt => stmt.pStmt'Access,
                                             pzTail => pzTail'Access);
      ICS.Free (c_sql);
      stmt.initialized := True;
      return (result = sqlite_h.SQLITE_OK);
   end prepare_sql;


   --------------------------------------------------------------------
   --  step_to_another_row #1
   --------------------------------------------------------------------
   function step_to_another_row (stmt : thick_stmt) return Boolean
   is
      use type IC.int;

      result : IC.int;
   begin
      result := sqlite_h.sqlite3_step (stmt.pStmt);
      return (result = sqlite_h.SQLITE_ROW);
   end step_to_another_row;


   --------------------------------------------------------------------
   --  step_to_completion #1
   --------------------------------------------------------------------
   function step_to_completion (stmt : thick_stmt) return Boolean
   is
      use type IC.int;

      result : IC.int;
   begin
      result := sqlite_h.sqlite3_step (stmt.pStmt);
      return (result = sqlite_h.SQLITE_DONE);
   end step_to_completion;


   --------------------------------------------------------------------
   --  step_to_another_row #2
   --------------------------------------------------------------------
   function step_to_another_row
     (stmt        : thick_stmt;
      num_retries : Natural) return Boolean
   is
      use type IC.int;

      result : IC.int;
      slpres : IC.int;
      counter : Natural := 0;
   begin
      loop
         result := sqlite_h.sqlite3_step (stmt.pStmt);
         exit when Integer (result) /= sqlite_h.enum_error_types'Pos (sqlite_h.SQLITE_BUSY);
         exit when counter > num_retries;
         counter := counter + 1;
         slpres := sqlite_h.sqlite3_sleep (IC.int (200));
      end loop;
      return (result = sqlite_h.SQLITE_ROW);
   end step_to_another_row;


   --------------------------------------------------------------------
   --  step_to_completion #2
   --------------------------------------------------------------------
   function step_to_completion
     (stmt        : thick_stmt;
      num_retries : Natural) return Boolean
   is
      use type IC.int;

      result : IC.int;
      slpres : IC.int;
      counter : Natural := 0;
   begin
      loop
         result := sqlite_h.sqlite3_step (stmt.pStmt);
         exit when Integer (result) /= sqlite_h.enum_error_types'Pos (sqlite_h.SQLITE_BUSY);
         exit when counter > num_retries;
         counter := counter + 1;
         slpres := sqlite_h.sqlite3_sleep (IC.int (200));
      end loop;
      return (result = sqlite_h.SQLITE_DONE);
   end step_to_completion;


   --------------------------------------------------------------------
   --  step
   --------------------------------------------------------------------
   function step (stmt : thick_stmt) return Step_Result
   is
      result : IC.int;
   begin
      result := sqlite_h.sqlite3_step (stmt.pStmt);
      case result is
         when sqlite_h.SQLITE_DONE =>
            return no_more_data;
         when sqlite_h.SQLITE_ROW =>
            return row_present;
         when others =>
            return something_else;
      end case;
   end step;


   --------------------------------------------------------------------
   --  retrieve_integer
   --------------------------------------------------------------------
   function retrieve_integer
     (stmt   : thick_stmt;
      column : Natural) return sql_int64
   is
      result : sqlite_h.sql64;
   begin
      result := sqlite_h.sqlite3_column_int64 (stmt.pStmt, IC.int (column));
      return sql_int64 (result);
   end retrieve_integer;


   --------------------------------------------------------------------
   --  retrieve_string
   --------------------------------------------------------------------
   function retrieve_string
     (stmt   : thick_stmt;
      column : Natural) return String
   is
      result : ICS.chars_ptr;
   begin
      --  Don't free result!
      result := sqlite_h.sqlite3_column_text (stmt.pStmt, IC.int (column));
      return translate_char_pointer (result);
   end retrieve_string;


   --------------------------------------------------------------------
   --  retrieve_boolean
   --------------------------------------------------------------------
   function retrieve_boolean
     (stmt   : thick_stmt;
      column : Natural) return Boolean
   is
      use type sqlite_h.sql64;

      result : sqlite_h.sql64;
   begin
      result := sqlite_h.sqlite3_column_int64 (stmt.pStmt, IC.int (column));
      return (result /= 0);
   end retrieve_boolean;


   --------------------------------------------------------------------
   --  column_data_type
   --------------------------------------------------------------------
   function column_data_type
     (stmt   : thick_stmt;
      column : Natural) return Data_Type
   is
      result : IC.int;
   begin
      result := sqlite_h.sqlite3_column_type (stmt.pStmt, IC.int (column));
      case result is
         when 1 => return SQL_INTEGER;
         when 2 => return SQL_FLOAT;
         when 3 => return SQL_TEXT;
         when 4 => return SQL_BLOB;
         when 5 => return SQL_NULL;
         when others =>
            raise unrecognized_column_type with result'Img;
      end case;
   end column_data_type;


   --------------------------------------------------------------------
   --  finalize_statement
   --------------------------------------------------------------------
   procedure finalize_statement (stmt : in out thick_stmt)
   is
      procedure free_string (Position : Char_Pointer_Crate.Cursor);
      procedure really_free (Element : in out ICS.chars_ptr);

      procedure free_string (Position : Char_Pointer_Crate.Cursor) is
      begin
         stmt.char_pointers.Update_Element (Position, really_free'Access);
      end free_string;

      procedure really_free (Element : in out ICS.chars_ptr) is
      begin
         ICS.Free (Element);
      end really_free;

      result : IC.int;
   begin
      if stmt.initialized then
         result := sqlite_h.sqlite3_finalize (stmt.pStmt);
         stmt.char_pointers.Iterate (free_string'Access);
         stmt.pStmt := null;
         stmt.initialized := False;
      end if;
   end finalize_statement;


   --------------------------------------------------------------------
   --  clear_bindings
   --------------------------------------------------------------------
   procedure clear_bindings (stmt : in out thick_stmt)
   is
      procedure free_string (Position : Char_Pointer_Crate.Cursor);
      procedure really_free (Element : in out ICS.chars_ptr);

      procedure free_string (Position : Char_Pointer_Crate.Cursor) is
      begin
         stmt.char_pointers.Update_Element (Position, really_free'Access);
      end free_string;

      procedure really_free (Element : in out ICS.chars_ptr) is
      begin
         ICS.Free (Element);
      end really_free;

      res : IC.int;
   begin
      stmt.char_pointers.Iterate (free_string'Access);
      stmt.char_pointers.Clear;
      res := sqlite_h.sqlite3_clear_bindings (stmt.pStmt);
   end clear_bindings;



   --------------------------------------------------------------------
   --  close_database
   --------------------------------------------------------------------
   procedure close_database (db : db3)
   is
      result : IC.int;
   begin
      result := sqlite_h.sqlite3_close (db);
   end close_database;


   --------------------------------------------------------------------
   --  shutdown_sqlite
   --------------------------------------------------------------------
   procedure shutdown_sqlite
   is
      result : IC.int;
   begin
      result := sqlite_h.sqlite3_shutdown;
   end shutdown_sqlite;


   --------------------------------------------------------------------
   --  sqlite3_get_auxdata_as_regex
   --------------------------------------------------------------------
   function sqlite3_get_auxdata_as_regex
     (context : db3_context;
      N       : Integer) return reg_expression
   is
      function convert is new Ada.Unchecked_Conversion (Source => sqlite_h.Void_Ptr,
                                                        Target => reg_expression);
   begin
      return convert (sqlite_h.sqlite3_get_auxdata (context, IC.int (N)));
   end sqlite3_get_auxdata_as_regex;


   --------------------------------------------------------------------
   --  sqlite3_set_auxdata_as_regex
   --------------------------------------------------------------------
   procedure sqlite3_set_auxdata_as_regex
     (context  : db3_context;
      N        : Integer;
      data     : reg_expression;
      callback : cb_regex)
   is
      function convert2void is new Ada.Unchecked_Conversion (Source => reg_expression,
                                                             Target => sqlite_h.Void_Ptr);
      function convert2callback is new Ada.Unchecked_Conversion (Source => cb_regex,
                                                                 Target => sqlite_h.cb_auxdata);

      data_ptr : sqlite_h.Void_Ptr;
      gen_cb   : sqlite_h.cb_auxdata;
   begin
      data_ptr := convert2void (data);
      gen_cb   := convert2callback (callback);

      sqlite_h.sqlite3_set_auxdata (context  => context,
                                    N        => IC.int (N),
                                    data     => data_ptr,
                                    callback => gen_cb);
   end sqlite3_set_auxdata_as_regex;


   --------------------------------------------------------------------
   --  db_connected
   --------------------------------------------------------------------
   function db_connected (db : db3) return Boolean
   is
      use type db3;
   begin
      return (db /= null);
   end db_connected;


   --------------------------------------------------------------------
   --  get_last_error_message
   --------------------------------------------------------------------
   function get_last_error_message
     (db : not null db3) return String
   is
      c_msg : ICS.chars_ptr;
   begin
      c_msg := sqlite_h.sqlite3_errmsg (db);
      return translate_char_pointer (c_msg);
   end get_last_error_message;


   --------------------------------------------------------------------
   --  get_last_error_code
   --------------------------------------------------------------------
   function get_last_error_code
     (db : not null db3) return db3_error
   is
      code : IC.int;
   begin
      code := sqlite_h.sqlite3_errcode (db);

      return sqlite_h.enum_error_types'Val (Integer (code) - 1);
   end get_last_error_code;


   --------------------------------------------------------------------
   --  invalid_regex
   --------------------------------------------------------------------
   function invalid_regex
     (db : not null db3) return Boolean
   is
   begin
      return (get_last_error_message (db) = "Invalid regex");
   end invalid_regex;


   --------------------------------------------------------------------
   --  last_exec_message
   --------------------------------------------------------------------
   function last_exec_message return String is
   begin
      return translate_char_pointer (last_errmsg);
   end last_exec_message;


   --------------------------------------------------------------------
   --  exec_sql
   --------------------------------------------------------------------
   function exec_sql
     (db  : not null db3;
      sql : String) return Boolean
   is
      use type IC.int;

      c_sql  : ICS.chars_ptr;
      res    : IC.int;
   begin
      c_sql := ICS.New_String (sql);
      res := sqlite_h.sqlite3_exec (db       => db,
                                    sql      => c_sql,
                                    callback => System.Null_Address,
                                    firstarg => System.Null_Address,
                                    errmsg   => last_errmsg'Address);
      ICS.Free (c_sql);
      return res = sqlite_h.SQLITE_OK;
   end exec_sql;


   --------------------------------------------------------------------
   --  database_was_opened_readonly
   --------------------------------------------------------------------
   function database_was_opened_readonly
     (db     : not null sqlite_h.sqlite3_Access;
      dbname : String) return Boolean
   is
      use type IC.int;
      res      : IC.int;
      c_dbname : ICS.chars_ptr;
   begin
      c_dbname := ICS.New_String (dbname);
      res := sqlite_h.sqlite3_db_readonly (db, c_dbname);
      ICS.Free (c_dbname);
      return (res = IC.int (1));
   end database_was_opened_readonly;


   --------------------------------------------------------------------
   --  get_sql
   --------------------------------------------------------------------
   function get_sql (pStmt : not null db3_stmt) return String
   is
      sql : ICS.chars_ptr;
   begin
      sql := sqlite_h.sqlite3_sql (pStmt);
      return translate_char_pointer (sql);
   end get_sql;


   ------------------------
   --  get_expanded_sql  --
   ------------------------
   function get_expanded_sql (stmt : thick_stmt) return String
   is
      sql : ICS.chars_ptr;
   begin
      sql := sqlite_h.sqlite3_expanded_sql (stmt.pStmt);
      return translate_char_pointer (sql);
   end get_expanded_sql;


   --------------------------------------------------------------------
   --  set_sqlite_profile
   --------------------------------------------------------------------
   procedure set_sqlite_profile
     (db       : not null sqlite_h.sqlite3_Access;
      callback : db3_trace)
   is
      res : IC.int;
   begin
      res := sqlite_h.sqlite3_trace_v2 (db       => db,
                                        uMask    => sqlite_h.SQLITE_TRACE_PROFILE,
                                        callback => callback,
                                        pCtx     => System.Null_Address);
   end set_sqlite_profile;


   --------------------------------------------------------------------
   --  reset_statement
   --------------------------------------------------------------------
   function reset_statement
     (stmt : thick_stmt) return Boolean
   is
      use type IC.int;
      res : IC.int;
   begin
      res := sqlite_h.sqlite3_reset (stmt.pStmt);
      return (res = sqlite_h.SQLITE_OK);
   end reset_statement;


   --------------------------------------------------------------------
   --  get_number_of_columns
   --------------------------------------------------------------------
   function get_number_of_columns
     (stmt : thick_stmt) return Integer
   is
      numcols : IC.int;
   begin
      numcols := sqlite_h.sqlite3_column_count (stmt.pStmt);
      return (Integer (numcols));
   end get_number_of_columns;


   --------------------------------------------------------------------
   --  get_column_name
   --------------------------------------------------------------------
   function get_column_name
     (stmt         : thick_stmt;
      column_index : Natural) return String
   is
      name : ICS.chars_ptr;
   begin
      --  Don't free result!
      name := sqlite_h.sqlite3_column_name (stmt.pStmt, IC.int (column_index));
      return translate_char_pointer (name);
   end get_column_name;


   --------------------------------------------------------------------
   --  bind_integer
   --------------------------------------------------------------------
   procedure bind_integer
     (stmt         : thick_stmt;
      column_index : Natural;
      value        : sql_int64)
   is
      c_value : constant sqlite_h.sql64 := sqlite_h.sql64 (value);
      res : IC.int;
   begin
      res := sqlite_h.sqlite3_bind_int64 (Handle => stmt.pStmt,
                                          Index  => IC.int (column_index),
                                          Value  => c_value);
   end bind_integer;


   --------------------------------------------------------------------
   --  bind_string
   --------------------------------------------------------------------
   procedure bind_string
     (stmt         : in out thick_stmt;
      column_index : Natural;
      value        : String)
   is
      txt : ICS.chars_ptr;
      res : IC.int;
   begin
      txt := ICS.New_String (value);
      res := sqlite_h.sqlite3_bind_text (Handle     => stmt.pStmt,
                                         Index      => IC.int (column_index),
                                         Text       => txt,
                                         nBytes     => IC.int (-1),
                                         destructor => sqlite_h.SQLITE_STATIC);
      stmt.char_pointers.Append (txt);
   end bind_string;


   --------------------------------------------------------------------
   --  get_db_filename
   --------------------------------------------------------------------
   function get_db_filename
     (db  : not null sqlite_h.sqlite3_Access;
      tag : String) return String
   is
      zdbname : ICS.chars_ptr;
      res     : ICS.chars_ptr;
   begin
      zdbname := ICS.New_String (tag);
      res := sqlite_h.sqlite3_db_filename (db, zdbname);
      ICS.Free (zdbname);

      --  don't free result!
      return translate_char_pointer (res);
   end get_db_filename;


   --------------------------------------------------------------------
   --  get_number_of_changes
   --------------------------------------------------------------------
   function get_number_of_changes
     (db : not null sqlite_h.sqlite3_Access) return Integer
   is
      res : IC.int;
   begin
      res := sqlite_h.sqlite3_changes (db);
      return (Integer (res));
   end get_number_of_changes;


   --------------------------------------------------------------------
   --  database_corrupt
   --------------------------------------------------------------------
   function database_corrupt
     (db : not null sqlite_h.sqlite3_Access) return Boolean
   is
      use type sqlite_h.enum_error_types;
   begin
      return (SQLite.get_last_error_code (db) = sqlite_h.SQLITE_CORRUPT);
   end database_corrupt;


   --------------------------------------------------------------------
   --  translate_char_pointer
   --------------------------------------------------------------------
   function translate_char_pointer (pointer : ICS.chars_ptr) return String
   is
      use type ICS.chars_ptr;
   begin
      if pointer = ICS.Null_Ptr then
         return "";
      else
         return ICS.Value (pointer);
      end if;
   end translate_char_pointer;


   --------------------------------------------------------------------
   --  set_sqlite_error
   --------------------------------------------------------------------
   procedure set_sqlite_error
     (context : db3_context;
      message : String;
      errnum  : Integer)
   is
      errmsg : ICS.chars_ptr;
   begin
      errmsg := ICS.New_String (message);
      sqlite_h.sqlite3_result_error (context, errmsg, IC.int (errnum));
      ICS.Free (errmsg);
   end set_sqlite_error;


   --------------------------------------------------------------------
   --  create_function
   --------------------------------------------------------------------
   procedure create_function
     (db    : not null db3;
      name  : String;
      nargs : Natural;
      cb    : db3_callback)
   is
      use type IC.int;
      flags : constant IC.int := sqlite_h.SQLITE_ANY + sqlite_h.SQLITE_DETERMINISTIC;
      result : IC.int;
   begin
      result := sqlite_h.sqlite3_create_function
        (db            => db,
         zFunctionName => ICS.New_String (name),
         nArg          => IC.int (nargs),
         eTextRep      => flags,
         pApp          => System.Null_Address,
         xFunc         => cb,
         xStep         => null,
         xFinal        => null);
   end create_function;


   -----------------
   --  get_value  --
   -----------------
   function get_value (value : db3_value) return String
   is
      value_ptr : ICS.chars_ptr;
   begin
      value_ptr := sqlite_h.sqlite3_value_text (value);
      return (ICS.Value (value_ptr));
   end get_value;


   --------------------------
   --  set_integer_result  --
   --------------------------
   procedure set_integer_result
     (context  : db3_context;
      result   : Integer) is
   begin
      sqlite_h.sqlite3_result_int (context, IC.int (result));
   end set_integer_result;


   ----------------------------
   --  set_integer64_result  --
   ----------------------------
   procedure set_integer64_result
     (context  : db3_context;
      result   : sql_int64)
   is
   begin
      sqlite_h.sqlite3_result_int64 (context, sqlite_h.sql64 (result));
   end set_integer64_result;


   ------------------------------------------
   --  set_text_result_without_destructor  --
   ------------------------------------------
   procedure set_text_result_without_destructor
     (context : db3_context;
      result  : String;
      termpos : integer := -1)
   is
      c_res : ICS.chars_ptr;
   begin
      c_res := ICS.New_String (result);
      sqlite_h.sqlite3_result_text (context    => context,
                                    result     => c_res,
                                    termpos    => IC.Int (termpos),
                                    destructor => null);
      ICS.Free (c_res);
   end set_text_result_without_destructor;


   ---------------------
   --  get_db_handle  --
   ---------------------
   function get_db_handle (context : db3_context) return db3 is
   begin
      return sqlite_h.sqlite3_context_db_handle (context);
   end get_db_handle;


   ---------------------
   --  regex_compile  --
   ---------------------
   function regex_compile
     (preg    : reg_expression;
      pattern : string;
      flags   : Natural) return Boolean
   is
      cflags : IC.int := IC.int (flags);
      res    : IC.int;
      regex  : ICS.chars_ptr;
   begin
      regex := ICS.New_String (pattern);
      res := regex_h.regcomp (preg, regex, cflags);
      ICS.Free (regex);
      case res is
         when 0      => return True;
         when others => return False;
      end case;
   end regex_compile;


   function regex_match_found
     (preg      : reg_expression;
      candidate : String) return Boolean
   is
      ret : IC.int;
      regex : ICS.chars_ptr;
   begin
      regex := ICS.New_String (candidate);
      ret := regex_h.regexec (preg   => preg,
                              regex  => regex,
                              nmatch => 0,
                              pmatch => null,
                              eflags => 0);
      ICS.Free (regex);
      case ret is
         when 0 => return True;
         when others => return False;
      end case;
   end regex_match_found;


   -----------------------
   --  free_expression  --
   -----------------------
   procedure free_expression (preg : reg_expression) is
   begin
      regex_h.regfree (preg);
   end free_expression;


   ------------------------
   --  set_busy_timeout  --
   ------------------------
   function set_busy_timeout
     (db        : db3;
      millisecs : Natural) return Boolean
   is
      res : IC.int;
   begin
      res := sqlite_h.sqlite3_busy_timeout (db, IC.int (millisecs));
      case res is
         when 0      => return True;
         when others => return False;
      end case;
   end set_busy_timeout;


   -----------------------------
   --  get_last_insert_rowid  --
   -----------------------------
   function get_last_insert_rowid (db : db3) return sql_int64
   is
      res : sqlite_h.sql64;
   begin
      res := sqlite_h.sqlite3_last_insert_rowid (db);
      raise do_not_use_this with "unreliable, current value =" & res'Img;
      return sql_int64 (res);
   end get_last_insert_rowid;


   ----------------------
   --  in_transaction  --
   ----------------------
   function in_transaction (db : db3) return Boolean
   is
      res : IC.int;
   begin
      res := sqlite_h.sqlite3_get_autocommit (db);
      case res is
         when 0      => return True;
         when others => return False;
      end case;
   end in_transaction;


end SQLite;
