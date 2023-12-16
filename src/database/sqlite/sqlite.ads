--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with sqlite_h;
with regex_h;


private with Ada.Containers.Vectors;
private with Interfaces.C.Strings;

package SQLite is

   subtype db3          is sqlite_h.sqlite3_Access;
   subtype db3_stmt     is sqlite_h.sqlite3_stmt_Access;
   subtype db3_error    is sqlite_h.enum_error_types;
   subtype db3_context  is sqlite_h.sqlite3_context_Access;
   subtype db3_trace    is sqlite_h.cb_trace;
   subtype db3_value    is sqlite_h.sqlite3_value_Access;
   subtype db3_routine  is sqlite_h.sqlite3_api_routines_Access;
   subtype db3_callback is sqlite_h.cb_xFuncStep;

   subtype reg_expression is regex_h.regex_t_Access;
   subtype regex_storage  is regex_h.regex_t;

   SQL_OK   : constant Natural := sqlite_h.SQLITE_OK;
   SQL_ROW  : constant Natural := sqlite_h.SQLITE_ROW;
   SQL_DONE : constant Natural := sqlite_h.SQLITE_DONE;

   REX_CASE   : constant Natural := Natural (regex_h.REG_EXTENDED) + Natural (regex_h.REG_NOSUB);
   REX_NOCASE : constant Natural := REX_CASE + Natural (regex_h.REG_ICASE);

   type sql_int64 is range -(2**63) .. +(2**63 - 1);
   type Step_Result is (row_present, no_more_data, something_else);
   type thick_stmt is private;

   primary_db_identity : constant String := "main";

   --  return True on success
   function initialize_sqlite return Boolean;

   --  return True on success, db is output
   function open_sqlite_database_readonly
     (path  : String;
      ppDB  :  not null access db3) return Boolean;

   --  return True on success, db is output
   function open_sqlite_database_readwrite
     (path  : String;
      ppDB  :  not null access db3) return Boolean;

   --  return True on success, stmt is output
   function prepare_sql
     (pDB    : not null db3;
      sql    : String;
      stmt   : out thick_stmt) return Boolean;

   --  Return True if row found after the step
   function step_to_another_row (stmt : thick_stmt) return Boolean;

   --  Return True if query done after the step (no data returned)
   function step_to_completion (stmt : thick_stmt) return Boolean;

   --  Step through prepared statement, returning indication of 3 possible results
   function step (stmt : thick_stmt) return Step_Result;

   --  Return True if row found after the step, attempt num_retries when SQLITE_BUSY encountered
   function step_to_another_row
     (stmt        : thick_stmt;
      num_retries : Natural) return Boolean;

   --  Return True if query done after the step, attempt num_retries when SQLITE_BUSY encountered
   function step_to_completion
     (stmt        : thick_stmt;
      num_retries : Natural) return Boolean;

   --  After stepping, return 64-bit integer from given column
   function retrieve_integer
     (stmt   : thick_stmt;
      column : Natural) return sql_int64;

   --  After stepping, return string from given column
   function retrieve_string
     (stmt   : thick_stmt;
      column : Natural) return String;

   --  After stepping, return boolean from given column
   function retrieve_boolean
     (stmt   : thick_stmt;
      column : Natural) return Boolean;

   --  Close statement after use, don't return result
   procedure finalize_statement (stmt : in out thick_stmt);

   --  Close an open database, don't check result
   procedure close_database (db : db3);

   --  Shutdown sqlite3
   procedure shutdown_sqlite;

   function sqlite3_get_auxdata_as_regex
     (context : db3_context;
      N       : Integer) return reg_expression;

   type cb_regex is access procedure
     (regex_ptr :  not null reg_expression);
   pragma Convention (C, cb_regex);

   procedure sqlite3_set_auxdata_as_regex
     (context  : db3_context;
      N        : Integer;
      data     : reg_expression;
      callback : cb_regex);

   function db_connected
     (db : db3) return Boolean;

   function get_last_error_message
     (db : not null db3) return String;

   function get_last_error_code
     (db : not null db3) return db3_error;

   function invalid_regex
     (db : not null db3) return Boolean;

   procedure rdb_syscall_overload;
   pragma Import (C, rdb_syscall_overload);

   function exec_sql
     (db  : not null db3;
      sql : String) return Boolean;

   function last_exec_message return String;

   function database_was_opened_readonly
     (db     : not null db3;
      dbname : String) return Boolean;

   function get_sql
     (pStmt : not null db3_stmt) return String;

   procedure set_sqlite_profile
     (db       : not null db3;
      callback : db3_trace);

   function reset_statement
     (stmt : thick_stmt) return Boolean;

   function get_number_of_columns
     (stmt : thick_stmt) return Integer;

   function get_column_name
     (stmt         : thick_stmt;
      column_index : Natural) return String;

   procedure bind_integer
     (stmt         : thick_stmt;
      column_index : Natural;
      value        : sql_int64);

   procedure bind_string
     (stmt         : in out thick_stmt;
      column_index : Natural;
      value        : String);

   procedure clear_bindings
     (stmt         : in out thick_stmt);

   function get_db_filename
     (db  : not null db3;
      tag : String) return String;

   function get_number_of_changes
     (db : not null db3) return Integer;

   function database_corrupt
     (db : not null db3) return Boolean;

   procedure set_sqlite_error
     (context : db3_context;
      message : String;
      errnum  : Integer);

   procedure create_function
     (db    : not null db3;
      name  : String;
      nargs : Natural;
      cb    : db3_callback);

   function get_value (value : db3_value) return String;

   procedure set_integer_result
     (context  : db3_context;
      result   : Integer);

   procedure set_integer64_result
     (context  : db3_context;
      result   : sql_int64);

   procedure set_text_result_without_destructor
     (context : db3_context;
      result  : String;
      termpos : integer := -1);

   function get_db_handle
     (context : db3_context) return db3;

   function regex_compile
     (preg    : reg_expression;
      pattern : string;
      flags   : Natural) return Boolean;

   function regex_match_found
     (preg      : reg_expression;
      candidate : String) return Boolean;

   procedure free_expression (preg : reg_expression);

   function set_busy_timeout
     (db        : db3;
      millisecs : Natural) return Boolean;

private

   last_errmsg : Interfaces.C.Strings.chars_ptr;

   package Char_Pointer_Crate is new Ada.Containers.Vectors
     (Element_Type => Interfaces.C.Strings.chars_ptr,
      Index_Type   => Natural,
      "="          => Interfaces.C.Strings."=");

   type thick_stmt is
      record
         pStmt         : aliased db3_stmt;
         char_pointers : Char_Pointer_Crate.Vector;
         initialized   : Boolean := False;
      end record;

   function translate_char_pointer (pointer : Interfaces.C.Strings.chars_ptr) return String;

end SQLite;
