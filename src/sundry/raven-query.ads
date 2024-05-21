--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Pkgtypes;

package Raven.Query is


   type A_Database is (installation_database, catalog_database);

   --  if all_packages is true, pattern needs to be blank
   --  if all_packages is false, pattern cannot be blank
   procedure query_package_database
     (database       : A_Database;
      selection      : String;
      conditions     : String;
      pattern        : String;
      all_packages   : Boolean;
      override_csens : Boolean;
      override_exact : Boolean);

private

   token_invalid_for_column : exception;
   control_char_found       : exception;

   type A_Token is
     (token_unrecognized,
      token_num_categories,
      token_num_dependencies,
      token_num_directories,
      token_num_files,
      token_num_groups,
      token_num_licenses,
      token_num_annotations,
      token_num_options,
      token_num_reverse_deps,
      token_num_shlibs_adj,
      token_num_shlibs_pro,
      token_num_shlibs_req,
      token_num_users,
      token_abi,
      token_automatic,
      token_comment,
      token_description,
      token_license_logic,
      token_maintainer,
      token_namebase,
      token_nsv,
      token_prefix,
      token_size_bytes,
      token_size_iec_units,
      token_subpackage,
      token_install_time,
      token_variant,
      token_www_site,
      token_ml_categories,
      token_ml_deps_namebase,
      token_ml_deps_nsv,
      token_ml_deps_spkg,
      token_ml_deps_variant,
      token_ml_deps_version,
      token_ml_directories,
      token_ml_files_path,
      token_ml_files_digest,
      token_ml_groups,
      token_ml_licenses,
      token_ml_notes_key,
      token_ml_notes_value,
      token_ml_opt_key,
      token_ml_opt_value,
      token_ml_rdep_namebase,
      token_ml_rdep_nsv,
      token_ml_rdep_spkg,
      token_ml_rdep_variant,
      token_ml_rdep_version,
      token_ml_shlibs_adj,
      token_ml_shlibs_pro,
      token_ml_shlibs_req,
      token_ml_users);

   type Column_Selection is array (A_Token'Range) of Boolean;
   type evaluation_type is (unsupported, numeric, textual);
   type State_Machine is
     (start,
      post_open,
      post_var_num,
      post_op_num,
      post_num,
      post_var_str,
      post_op_str,
      post_str,
      post_close,
      done);

   type Condition_Box is
      record
         machine      : State_Machine := start;
         parens_open  : Natural := 0;
         where_clause : Text := SU.Null_Unbounded_String;
      end record;

   function get_token (component : String) return A_Token;

   --  Returns package column name or subquery to get package data from other tables.
   --  Will throw token_invalid_for_column for tokens that are not supported.
   function get_column (token : A_Token) return String;

   procedure tokenize
     (selection        : String;
      selection_tokens : in out Pkgtypes.Text_List.Vector;
      columns          : in out Column_Selection;
      num_columns      : out Natural);

   --  Returns True when token points to modifier value for evaluations
   function valid_for_where_clause (token : A_Token) return Boolean;

   --  Returns 'unsupported' for tokens not suitable for evaluation clause
   --  Otherwise the token is classified as numerical or string-based, used to determine
   --  if the next operator is valid
   function next_operator (token : A_Token) return evaluation_type;

   --  Adds spaces around specific characters to help with parsing
   --  Masks spaces that are inside single quotes
   function expand_original_condition (original : String) return String;

   --  Pass the next token and the current state of the machine to determine if the next
   --  token is valid.
   function next_token_valid
     (machine    : State_Machine;
      next_token : String) return Boolean;

   --  Build up internal condition clause based on machine state and token
   procedure process_token
     (processor  : in out Condition_Box;
      next_token : String);

   --  Ensures string is wrapped with escaped single quotes.
   function encode_string_operand (raw : String) return String;

   --  Returns true if found tokens are valid.  If "{" found without a mate, that
   --  will also cause a false return
   function evaluate_conditions_template
     (conditions : String;
      error_hit  : out Boolean) return String;

end Raven.Query;
