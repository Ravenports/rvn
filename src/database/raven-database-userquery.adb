--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Characters.Latin_1;
with SQLite;
with Raven.Event;
with Raven.Context;
with Raven.Pkgtypes;
with Raven.Metadata;
with Raven.Cmd.Unset;
with Raven.Database.CommonSQL;
with Raven.Strings;  use Raven.Strings;

package body Raven.Database.UserQuery is

   package LAT renames Ada.Characters.Latin_1;
   package RCU renames Raven.Cmd.Unset;

   -----------------
   --  get_token  --
   -----------------
   function get_token (component : String) return A_Token
   is
      --  the binary search is case insenstive

      total_keywords : constant Positive := A_Token'Pos (A_Token'Last) + 1;

      subtype keyword_string is String (1 .. 10);

      type keyword_pair is
         record
            keyword : keyword_string;
            keytype : A_Token;
         end record;

      --  Keep in alphabetical order (critical!)
      all_keywords : constant array (1 .. total_keywords) of keyword_pair :=
        (
         ("#NOTFOUND ", token_unrecognized),
         ("#cats     ", token_num_categories),
         ("#deps     ", token_num_dependencies),
         ("#dirs     ", token_num_directories),
         ("#files    ", token_num_files),
         ("#groups   ", token_num_groups),
         ("#lics     ", token_num_licenses),
         ("#msgs     ", token_num_messages),
         ("#notes    ", token_num_annotations),
         ("#opts     ", token_num_options),
         ("#rdeps    ", token_num_reverse_deps),
         ("#shadj    ", token_num_shlibs_adj),
         ("#shpro    ", token_num_shlibs_pro),
         ("#shreq    ", token_num_shlibs_req),
         ("#users    ", token_num_users),
         ("abi       ", token_abi),
         ("auto      ", token_automatic),
         ("comment   ", token_comment),
         ("desc      ", token_description),
         ("logic     ", token_license_logic),
         ("maint     ", token_maintainer),
         ("name      ", token_namebase),
         ("nsv       ", token_nsv),
         ("prefix    ", token_prefix),
         ("size:bytes", token_size_bytes),
         ("size:iec  ", token_size_iec_units),
         ("subpkg    ", token_subpackage),
         ("timestamp ", token_install_time),
         ("variant   ", token_variant),
         ("version   ", token_version),
         ("www       ", token_www_site),
         ("xcat      ", token_ml_categories),
         ("xdep:n    ", token_ml_deps_namebase),
         ("xdep:nsv  ", token_ml_deps_nsv),
         ("xdep:s    ", token_ml_deps_spkg),
         ("xdep:v    ", token_ml_deps_variant),
         ("xdep:ver  ", token_ml_deps_version),
         ("xdir      ", token_ml_directories),
         ("xfile:path", token_ml_files_path),
         ("xfile:sum ", token_ml_files_digest),
         ("xgroup    ", token_ml_groups),
         ("xlic      ", token_ml_licenses),
         ("xmsgi     ", token_ml_msg_install),
         ("xmsgr     ", token_ml_msg_remove),
         ("xmsgu     ", token_ml_msg_upgrade),
         ("xnote:key ", token_ml_notes_key),
         ("xnote:val ", token_ml_notes_value),
         ("xopt:key  ", token_ml_opt_key),
         ("xopt:val  ", token_ml_opt_value),
         ("xrdep:n   ", token_ml_rdep_namebase),
         ("xrdep:nsv ", token_ml_rdep_nsv),
         ("xrdep:s   ", token_ml_rdep_spkg),
         ("xrdep:v   ", token_ml_rdep_variant),
         ("xrdep:ver ", token_ml_rdep_version),
         ("xshadj    ", token_ml_shlibs_adj),
         ("xshpro    ", token_ml_shlibs_pro),
         ("xshreq    ", token_ml_shlibs_req),
         ("xuser     ", token_ml_users)
        );

      bandolier : keyword_string := (others => ' ');
      Low       : Natural := all_keywords'First;
      High      : Natural := all_keywords'Last;
      Mid       : Natural;
   begin
      if component'Length > keyword_string'Length or else
        component'Length < 3
      then
         return token_unrecognized;
      end if;

      bandolier (1 .. component'Length) := lowercase (component);

      loop
         Mid := (Low + High) / 2;
         if bandolier = all_keywords (Mid).keyword  then
            return all_keywords (Mid).keytype;
         elsif bandolier < all_keywords (Mid).keyword then
            exit when Low = Mid;
            High := Mid - 1;
         else
            exit when High = Mid;
            Low := Mid + 1;
         end if;
      end loop;
      return token_unrecognized;
   end get_token;


   ------------------
   --  get_column  --
   ------------------
   function get_column (token : A_Token) return String
   is
      id  : constant String := "package_id";
      rdep_subquery : constant String :=
        "(select count(xx.package_id) from pkg_dependencies xx " &
        "join dependencies dd on xx.dependency_id = dd.dependency_id " &
        "where dd.nsv = p.namebase ||'~'|| p.subpackage ||'~'|| p.variant)";

      function count_subquery (table_name, id_name : String) return String is
      begin
         --  Don't set "AS something" so it can be used in the WHERE clause
         return "(select count(" & id_name & ") from " & table_name &
           " where " & table_name & "." & id_name & " = p.id)"; -- as " & token'Img;
      end count_subquery;
   begin
      case token is
         when token_unrecognized => raise token_invalid_for_column;

         when token_num_categories   => return count_subquery ("pkg_categories", id);
         when token_num_dependencies => return count_subquery ("pkg_dependencies", id);
         when token_num_directories  => return count_subquery ("pkg_directories", id);
         when token_num_files        => return count_subquery ("pkg_files", id);
         when token_num_groups       => return count_subquery ("pkg_groups", id);
         when token_num_licenses     => return count_subquery ("pkg_licenses", id);
         when token_num_annotations  => return count_subquery ("pkg_annotations", id);
         when token_num_options      => return count_subquery ("pkg_options", id);
         when token_num_shlibs_adj   => return count_subquery ("pkg_libs_adjacent", id);
         when token_num_shlibs_pro   => return count_subquery ("pkg_libs_required", id);
         when token_num_shlibs_req   => return count_subquery ("pkg_libs_provided", id);
         when token_num_users        => return count_subquery ("pkg_users", id);
         when token_num_messages     => return count_subquery ("pkg_messages", id);
         when token_num_reverse_deps => return rdep_subquery;
         when token_abi              => return "p.abi";
         when token_automatic        => return "p.automatic";
         when token_comment          => return "p.comment";
         when token_description      => return "p.desc";
         when token_license_logic    => return "p.licenselogic";
         when token_maintainer       => return "p.maintainer";
         when token_namebase         => return "p.namebase";
         when token_nsv              => return nsv_formula;
         when token_prefix           => return "p.prefix";
         when token_size_iec_units   |  --  post-process
              token_size_bytes       => return "p.flatsize";
         when token_subpackage       => return "p.subpackage";
         when token_install_time     => return "p.installed";
         when token_variant          => return "p.variant";
         when token_version          => return "p.version";
         when token_www_site         => return "p.www";
            --  The remaining enumerations require joins (limited to one per unique table)
         when token_ml_categories    => return "ml.name";
         when token_ml_deps_namebase |  --  post-process
              token_ml_deps_variant  |  --  post-process
              token_ml_deps_spkg     |  --  post-process
              token_ml_deps_nsv      => return "ml.nsv";
         when token_ml_deps_version  => return "ml.version";
         when token_ml_directories   => return "ml.path";
         when token_ml_files_path    => return "ml.path";
         when token_ml_files_digest  => return "ml.b3digest";
         when token_ml_groups        => return "ml.name";
         when token_ml_licenses      => return "ml.name";
         when token_ml_notes_key     => return "ml.note_key";
         when token_ml_notes_value   => return "x.annotation";
         when token_ml_opt_key       => return "ml.option_name";
         when token_ml_opt_value     => return "x.option_setting";
         when token_ml_rdep_namebase => return "p.namebase";
         when token_ml_rdep_spkg     => return "p.subpackage";
         when token_ml_rdep_variant  => return "p.variant";
         when token_ml_rdep_nsv      => return nsv_formula;
         when token_ml_rdep_version  => return "p.version";
         when token_ml_shlibs_adj    => return "ml.name";
         when token_ml_shlibs_pro    => return "ml.name";
         when token_ml_shlibs_req    => return "ml.name";
         when token_ml_users         => return "ml.name";
         when token_ml_msg_install   |
              token_ml_msg_remove    |
              token_ml_msg_upgrade   => return "ml.message";
      end case;
   end get_column;


   ------------------------------
   --  valid_for_where_clause  --
   ------------------------------
   function valid_for_where_clause (token : A_Token) return Boolean is
   begin
      case token is
         when token_unrecognized   |
              token_size_iec_units |
              token_license_logic  |
              token_ml_categories .. token_ml_users =>
            return False;
         when others =>
            return True;
      end case;
   end valid_for_where_clause;


   ---------------------
   --  next_operator  --
   ---------------------
   function next_operator (token : A_Token) return evaluation_type is
   begin
      case token is
         when token_unrecognized |
              token_size_iec_units |
              token_ml_categories .. token_ml_msg_upgrade => return unsupported;
         when token_num_categories .. token_num_messages |
              token_size_bytes |
              token_automatic |
              token_install_time => return numeric;
         when token_abi |
              token_subpackage |
              token_variant |
              token_version |
              token_www_site |
              token_comment .. token_prefix => return textual;
      end case;
   end next_operator;


   ----------------
   --  tokenize  --
   ----------------
   procedure tokenize
     (selection        : String;
      selection_tokens : in out Pkgtypes.Text_List.Vector;
      columns          : in out Column_Selection;
      num_columns      : out Natural)
   is
      num_left_braces : Natural := 0;
      left_brace  : constant String (1 .. 1) := (others => LAT.Left_Curly_Bracket);
      right_brace : constant String (1 .. 1) := (others => LAT.Right_Curly_Bracket);

      procedure push (fragment : String) is
      begin
         Event.emit_debug (low_level, "tokenize: push '" & fragment & "'");
         selection_tokens.Append (SUS (fragment));
      end push;

      procedure no_replacement_push (field_number : Natural; fragment : String) is
      begin
         if field_number > 1 then
            push ('{' & fragment);
         elsif fragment'Length > 0 then
            push (fragment);
         end if;
      end no_replacement_push;
   begin
      selection_tokens.clear;
      columns := (others => False);
      num_columns := 0;
      num_left_braces := count_char (selection, LAT.Left_Curly_Bracket);

      if num_left_braces = 0 then
         --  no placeholders found.  Probably a user mistake.
         push (selection);
         return;
      end if;

      for field_number in 1 .. num_left_braces + 1 loop
         declare
            field : constant String := specific_field (selection, field_number, left_brace);
            num_right_braces : Natural;
         begin
            if field'Length < 4 then
               no_replacement_push (field_number, field);
            else
               --  We are between '{' characters.
               --  There might be a '}' here.  If there is, check to see if it's a
               --  recognized token.  If not, put the entire selection as text.
               --  Otherwise split it into two.
               num_right_braces := count_char (field, LAT.Right_Curly_Bracket);
               if num_right_braces = 0 then
                     no_replacement_push (field_number, field);
               else
                  declare
                     left_field  : constant String := part_1 (field, right_brace);
                     right_field : constant String := part_2 (field, right_brace);
                     column      : A_Token;
                  begin
                     column := get_token (left_field);
                     case column is
                        when token_unrecognized =>
                              no_replacement_push (field_number, field);
                        when others =>
                           if not columns (column) then
                              num_columns := num_columns + 1;
                              columns (column) := True;
                           end if;
                           push (left_field);
                           push (right_field);
                     end case;
                  end;
               end if;
            end if;
         end;
      end loop;

   end tokenize;


   ---------------------------------
   --  expand_original_condition  --
   ---------------------------------
   function expand_original_condition (original : String) return String
   is
      quote_open : Boolean := false;
      result : Text := Strings.blank;
      c : Character;
   begin
      for ndx in original'Range loop
         c := original (ndx);
         if quote_open then
            case c is
               when Character'Val (0) .. Character'Val (31) =>
                  raise control_char_found;
               when LAT.Space =>
                  SU.Append (result, Character'Val (0));
               when LAT.Apostrophe =>
                  SU.Append (result, c & LAT.Space);
                  quote_open := False;
               when others =>
                  SU.Append (result, c);
            end case;
         else
            case c is
               when LAT.Vertical_Line |
                    LAT.Ampersand |
                    LAT.Left_Parenthesis |
                    LAT.Right_Parenthesis =>
                  SU.Append (result, LAT.Space & c & LAT.Space);
               when LAT.Left_Curly_Bracket =>
                  SU.Append (result, LAT.space & c);
               when LAT.Right_Curly_Bracket =>
                  SU.Append (result, c & LAT.Space);
               when LAT.Apostrophe =>
                  SU.Append (result, LAT.Space & c);
                  quote_open := True;
               when Character'Val (0) .. Character'Val (31) =>
                  raise control_char_found;
               when others =>
                  SU.Append (result, c);
            end case;
         end if;
      end loop;
      return USS (result);
   end expand_original_condition;


   ------------------------------------
   --  evaluate_conditions_template  --
   ------------------------------------
   function evaluate_conditions_template (conditions : String;
                                          error_hit : out Boolean) return String
   is
      func : constant String := "evaluate_conditions_template: ";
      errmsg : constant String := "evaluation problem: ";
   begin
      error_hit := False;
      if IsBlank (trim (conditions)) then
         return "";
      end if;
      declare
         num_spaces : Natural;
         spaced_out : constant String := expand_original_condition (conditions);
         processor  : Condition_Box;
      begin
         num_spaces := count_char (spaced_out, LAT.Space);
         for field_num in 1 .. num_spaces + 1 loop
            declare
               field : constant String := specific_field (spaced_out, field_num);
            begin
               if field'Length > 0 then
                  if next_token_valid (processor.machine, field) then
                     process_token (processor, field);
                  else
                     error_hit := True;
                     case processor.machine is
                        when start | post_open =>
                           Event.emit_error (errmsg & "'" & field & "' not recognized");
                        when post_var_num =>
                           Event.emit_error (errmsg & "invalid operator for number comparison");
                        when post_var_str =>
                           Event.emit_error (errmsg & "invalid operator for string comparison");
                        when post_op_num =>
                           Event.emit_error (errmsg & "'" & field & "' is not numeric");
                        when post_num | post_str | post_close =>
                           Event.emit_error (errmsg & "'" & field & "', but expected &,|,)");
                        when post_op_str | done =>
                           null;  --  impossible
                     end case;
                     Event.emit_debug (moderate, "invalid token '" & field & "' for state " &
                                         processor.machine'Img);
                     return "invalid token";
                  end if;
               end if;
            end;
         end loop;
         if processor.parens_open = 0 then
            case processor.machine is
               when start | post_num | post_str | post_close | done =>
                  processor.machine := done;
               when others => null;
            end case;
         end if;

         case processor.machine is
            when done =>
               return USS (processor.where_clause);
            when others =>
               error_hit := True;
               Event.emit_error (errmsg & "clause is incomplete");
               Event.emit_debug
                 (moderate, "no more tokens, but the evaluation close is incomplete");
               return "incomplete";
         end case;
      end;
   exception
      when control_char_found =>
         error_hit := True;
         Event.emit_error (errmsg & "illegal control character found");
         Event.emit_debug (moderate, "condition clause had illegal control characters in it");
         return "control char exception";
   end evaluate_conditions_template;


   ------------------------
   --  next_token_valid  --
   ------------------------
   function next_token_valid
     (machine : State_Machine;
      next_token : String) return Boolean
   is
   begin
      case machine is
         when start | post_open =>
            if next_token = "(" then
               return True;
            end if;
            --  Should be {token}, but the curly braces need to be stripped to check.
            --  Smallest next_token can be is 5 characters long
            if next_token'Length < 5 then
               return False;
            end if;
            declare
               candidate  : constant String :=
                            next_token (next_token'First + 1 .. next_token'Last - 1);
               test_token : constant A_Token := get_token (candidate);
               can_eval : constant evaluation_type := next_operator (test_token);
            begin
               case can_eval is
                  when unsupported => return False;
                  when numeric | textual => return True;
               end case;
            end;
         when post_var_num =>
            return
              uppercase (next_token) = "EQ" or else
              uppercase (next_token) = "NE" or else
              next_token = ">=" or else
              next_token = "<=" or else
              next_token = ">" or else
              next_token = "<";
         when post_var_str =>
            return
              next_token = "~" or else
              next_token = "=" or else
              next_token = "^" or else
              next_token = "!~" or else
              next_token = "!=" or else
              next_token = "!^";
         when post_op_num =>
            return IsNumeric (next_token);
         when post_op_str =>
            return True;
         when post_num | post_str | post_close =>
            return next_token = "&" or else
              next_token = "|" or else
              next_token = ")";
         when done =>
            return False;
      end case;
   end next_token_valid;


   ---------------------
   --  process_token  --
   ---------------------
   procedure process_token
     (processor  : in out Condition_Box;
      next_token : String)
   is
   begin
      case processor.machine is
         when start | post_open =>
            if next_token = "(" then
               SU.Append (processor.where_clause, next_token);
               processor.parens_open := processor.parens_open + 1;
               processor.machine := post_open;
               return;
            end if;
            --  must be a valid token
            declare
               candidate  : constant String :=
                            next_token (next_token'First + 1 .. next_token'Last - 1);
               test_token : constant A_Token := get_token (candidate);
               can_eval : constant evaluation_type := next_operator (test_token);
            begin
               case can_eval is
                  when unsupported => null;  -- impossible
                  when numeric =>
                     processor.machine := post_var_num;
                  when textual =>
                     processor.machine := post_var_str;
               end case;
               SU.Append (processor.where_clause, get_column (test_token));
            end;
         when post_var_num =>
            if uppercase (next_token) = "EQ" then
               SU.append (processor.where_clause, " = ");
            elsif uppercase (next_token) = "NE" then
               SU.append (processor.where_clause, " <> ");
            else
               SU.Append (processor.where_clause, LAT.Space & next_token & LAT.Space);
            end if;
            processor.machine := post_op_num;
         when post_var_str =>
            if next_token = "~" then
               SU.Append (processor.where_clause, " GLOB ");
            elsif next_token = "!~" then
              SU.Append (processor.where_clause, " NOT GLOB ");
            elsif next_token = "=" then
               SU.Append (processor.where_clause, " = ");
            elsif next_token = "!=" then
               SU.Append (processor.where_clause, " <> ");
            elsif next_token = "^" then
               SU.Append (processor.where_clause, " LIKE ");
            elsif next_token = "!^" then
               SU.Append (processor.where_clause, " NOT LIKE ");
            end if;
            processor.machine := post_op_str;
         when post_op_num =>
            SU.Append (processor.where_clause, next_token);
            processor.machine := post_num;
         when post_op_str =>
            SU.Append (processor.where_clause, encode_string_operand (next_token));
            processor.machine := post_str;
         when post_num | post_str | post_close =>
            if next_token = ")" then
               SU.Append (processor.where_clause, next_token);
               processor.parens_open := processor.parens_open - 1;
               processor.machine := post_close;
            elsif next_token = "|" then
               SU.Append (processor.where_clause, " OR ");
               processor.machine := start;
            elsif next_token = "&" then
               SU.Append (processor.where_clause, " AND ");
               processor.machine := start;
            end if;
         when done =>
            null;
      end case;
   end process_token;


   -----------------------------
   --  encode_string_operand  --
   -----------------------------
   function encode_string_operand (raw : String) return String
   is
      num_quote : constant Natural := count_char (raw, LAT.Apostrophe);

      function escape_quote (dirty : String) return String
      is
         --  guaranteed dirty_length is at least 1
         num_replacements : constant Natural := count_char (dirty, LAT.Apostrophe);
         canvas : string (1 .. dirty'Length + num_replacements);
         index : Natural := canvas'First;
      begin
         for x in dirty'Range loop
            case dirty(x) is
               when LAT.Apostrophe =>
                  canvas (index) := LAT.Apostrophe;
                  index := index + 1;
                  canvas (index) := LAT.Apostrophe;
               when others =>
                  canvas (index) := dirty(x);
            end case;
            index := index + 1;
         end loop;
         return canvas;
      end escape_quote;
   begin
      if num_quote = 0 then
         return LAT.Apostrophe & raw & LAT.Apostrophe;
      end if;

      if raw (raw'First) = LAT.Apostrophe and then
        raw (raw'Last) = LAT.Apostrophe
      then
         if num_quote = 1 then
            return "''''";
         end if;

         if num_quote = 2 then
            return raw;
         end if;

         --  num_quote > 2, meaning at least one internal character is a quote
         --  that's pretty much a user error but it would break to query
         return LAT.Apostrophe & escape_quote (raw(raw'First + 1 .. raw'Last - 1)) & LAT.Apostrophe;

      end if;

      return escape_quote (raw);
   end;


   --------------------------------
   --  number_multiline_columns  --
   --------------------------------
   function number_multiline_columns (columns : Column_Selection) return Natural
   is
      multiline : Natural := 0;

      --  Some groups of ML data are considered "1" column since they all use the same join
      type mlc is
        (depends, revdeps, categories, files, directories, options, licenses, users, groups,
         libsreq, libsprov, libsadj, notes, msgs_install, msgs_remove, msgs_upgrade);
      seen : array (mlc'Range) of Boolean;
   begin
      seen := (others => False);
      for x in A_Token'Range loop
         if columns (x) then
            case x is
               when token_ml_categories => seen (categories) := True;
               when token_ml_deps_namebase .. token_ml_deps_version => seen (depends) := True;
               when token_ml_directories => seen (directories) := True;
               when token_ml_files_path .. token_ml_files_digest => seen (files) := True;
               when token_ml_groups => seen (groups) := True;
               when token_ml_licenses => seen (licenses) := True;
               when token_ml_notes_key .. token_ml_notes_value => seen (notes) := True;
               when token_ml_opt_key .. token_ml_opt_value => seen (options) := True;
               when token_ml_rdep_namebase .. token_ml_rdep_version => seen (revdeps) := True;
               when token_ml_shlibs_adj => seen (libsadj) := True;
               when token_ml_shlibs_pro => seen (libsprov) := True;
               when token_ml_shlibs_req => seen (libsreq) := True;
               when token_ml_users => seen (users) := True;
               when token_ml_msg_install => seen (msgs_install) := True;
               when token_ml_msg_remove  => seen (msgs_remove)  := True;
               when token_ml_msg_upgrade => seen (msgs_upgrade) := True;
               when others => null;
            end case;
         end if;
      end loop;

      for x in mlc'Range loop
         if seen (x) then
            multiline := multiline + 1;
         end if;
      end loop;
      return multiline;

   end number_multiline_columns;



   ------------------------------
   --  multicolumn_join_lines  --
   ------------------------------
   function multicolumn_join_lines (columns : Column_Selection) return String
   is
      ml_column : A_Token := token_unrecognized;
   begin
      for x in A_Token'Range loop
         case x is
            when token_ml_categories .. token_ml_msg_upgrade =>
               if columns (x) then
                  ml_column := x;
                  exit;
               end if;
            when others => null;
         end case;
      end loop;
      case ml_column is
         when token_ml_categories =>
            return
              " JOIN pkg_categories x on x.package_id = p.id" &
              " JOIN categories ml on ml.category_id = x.category_id";
         when token_ml_deps_namebase |
              token_ml_deps_spkg     |
              token_ml_deps_variant  |
              token_ml_deps_nsv      |
              token_ml_deps_version  =>
            return
              " JOIN pkg_dependencies x on x.package_id = p.id" &
              " JOIN dependencies ml on ml.dependency_id = x.dependency_id";
         when token_ml_rdep_namebase |
              token_ml_rdep_spkg     |
              token_ml_rdep_variant  |
              token_ml_rdep_nsv      |
              token_ml_rdep_version  =>
            return "";  --  handled with second query
         when token_ml_directories =>
            return
              " JOIN pkg_directories x on x.package_id = p.id" &
              " JOIN directories ml on ml.directory_id = x.directory_id";
         when token_ml_files_digest |
              token_ml_files_path   =>
            return
              " JOIN pkg_files ml on ml.package_id = p.id";
         when token_ml_groups =>
            return
              " JOIN pkg_groups x on x.package_id = p.id" &
              " JOIN groups ml on ml.group_id = x.group_id";
         when token_ml_licenses =>
            return
              " JOIN pkg_licenses x on x.package_id = p.id" &
              " JOIN licenses ml on ml.license_id = x.license_id";
         when token_ml_notes_key |
              token_ml_notes_value =>
            return
              " JOIN pkg_annotations x on x.package_id = p.id" &
              " JOIN annotations ml on ml.annotation_id = x.annotation_id";
         when token_ml_opt_key |
              token_ml_opt_value =>
            return
              " JOIN pkg_options x on x.package_id = p.id" &
              " JOIN options ml on ml.option_id = x.option_id";
         when token_ml_shlibs_adj =>
            return
              " JOIN pkg_libs_adjacent x on x.package_id = p.id" &
              " JOIN libraries ml on ml.library_id = x.library_id";
         when token_ml_shlibs_pro =>
            return
              " JOIN pkg_libs_provided x on x.package_id = p.id" &
              " JOIN libraries ml on ml.library_id = x.library_id";
         when token_ml_shlibs_req =>
            return
              " JOIN pkg_libs_required x on x.package_id = p.id" &
              " JOIN libraries ml on ml.library_id = x.library_id";
         when token_ml_users =>
            return
              " JOIN pkg_users x on x.package_id = p.id" &
              " JOIN users ml on ml.user_id = x.user_id";
         when token_ml_msg_install |
              token_ml_msg_remove  |
              token_ml_msg_upgrade =>
            return
              " JOIN pkg_messages ml on ml.package_id = p.id";
         when others => null;
      end case;

      return " IMPOSSIBLE";
   end multicolumn_join_lines;


   ------------------------------
   --  query_package_database  --
   ------------------------------
   function query_package_database
     (db             : in out RDB_Connection;
      selection      : String;
      conditions     : String;
      pattern        : String;
      all_packages   : Boolean;
      override_exact : Boolean) return Boolean
   is
      selection_tokens : Pkgtypes.Text_List.Vector;
      columns          : Column_Selection;
      num_columns      : Natural;
      error_hit        : Boolean;
      reverse_deps     : Boolean;
      num_multi        : Natural;
      leading_match    : Boolean := False;
      sql : Text := SUS ("select " & nsv_formula & " as nsv000");
   begin
      tokenize (selection, selection_tokens, columns, num_columns);
      reverse_deps :=
        columns (token_ml_rdep_namebase) or else
        columns (token_ml_rdep_nsv) or else
        columns (token_ml_rdep_spkg) or else
        columns (token_ml_rdep_variant) or else
        columns (token_ml_rdep_version);
      num_multi := number_multiline_columns (columns);
      if num_multi > 1 then
         Event.emit_error ("Limit of 1 multiline pattern exceeded");
         return False;
      end if;
      if num_columns > 0 then
         for x in A_Token'Range loop
            if columns (x) then
               SU.Append (sql, ", " & get_column (x));
            end if;
         end loop;
      end if;
      SU.Append (sql, " FROM packages as p");

      if num_multi > 0 then
         SU.append (sql, multicolumn_join_lines (columns));
      end if;

      if conditions = "" then
         SU.Append (sql, " WHERE (1)");
      else
         declare
            populated : constant String := evaluate_conditions_template (conditions, error_hit);
         begin
            if error_hit then
               return False;
            end if;
            SU.Append (sql, " WHERE (" & populated & ")");
         end;
      end if;

      if columns (token_ml_msg_install) then
         SU.Append (sql, " AND ml.message_type = 0");
      elsif columns (token_ml_msg_remove) then
         SU.Append (sql, " AND ml.message_type = 1");
      elsif columns (token_ml_msg_upgrade) then
         SU.Append (sql, " AND ml.message_type = 2");
      end if;

      if not all_packages then
         if override_exact then
            SU.Append (sql, " AND nsv000 = ?");
         elsif Context.reveal_case_sensitive then
            SU.Append (sql, " AND nsv000 GLOB ?");
         else
            SU.Append (sql, " AND nsv000 LIKE ?");
            leading_match := True;
         end if;
      end if;

      declare
         func     : constant String := "query_package_database";
         rev_sql  : constant String :=
           "SELECT p.namebase, p.subpackage, p.variant, p.version, " &
           "       p.namebase ||'~'|| p.subpackage ||'~'|| p.variant as nsv " &
           "FROM packages as p JOIN pkg_dependencies x on x.package_id = p.id " &
           "WHERE x.dependency_id = (SELECT d.dependency_id FROM dependencies d WHERE nsv = ?) " &
           "ORDER by nsv";
         new_stmt : SQLite.thick_stmt;
         rev_stmt : SQLite.thick_stmt;
      begin
         if not SQLite.prepare_sql (db.handle, USS (sql), new_stmt) then
            Database.CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func, USS (sql));
            return False;
         end if;
         if reverse_deps then
            if not SQLite.prepare_sql (db.handle, rev_sql, rev_stmt) then
               Database.CommonSQL.ERROR_STMT_SQLITE
                 (db.handle, internal_srcfile, func, rev_sql);
               return False;
            end if;
         end if;
         if not all_packages then
            if leading_match then
               SQLite.bind_string (new_stmt, 1, pattern & '%');
            else
               SQLite.bind_string (new_stmt, 1, pattern);
            end if;
         end if;
         debug_running_stmt (new_stmt);

         loop
            case SQLite.step (new_stmt) is
               when SQLite.no_more_data => exit;
               when SQLite.row_present =>
                  declare
                     col_index : Natural := 0;  -- base nsv
                     result    : Column_Result;
                     outline   : Text := blank;

                     procedure assemble (Position : Pkgtypes.Text_List.Cursor)
                     is
                        component : constant String := USS (Pkgtypes.Text_List.Element (Position));
                        token     : constant A_Token := get_token (component);
                        delim     : constant String (1 .. 1) := (1 => LAT.Tilde);
                     begin
                        case token is
                           when token_unrecognized =>
                              SU.Append (outline, component);
                           when token_ml_deps_namebase =>
                              SU.Append (outline, specific_field (USS (result (token)), 1, delim));
                           when token_ml_deps_spkg =>
                              SU.Append (outline, specific_field (USS (result (token)), 2, delim));
                           when token_ml_deps_variant =>
                              SU.Append (outline, specific_field (USS (result (token)), 3, delim));
                           when token_size_iec_units =>
                              SU.Append (outline, Metadata.human_readable_size
                                         (int64'Value (USS (result (token)))));
                           when token_license_logic =>
                              SU.Append (outline, Metadata.get_license_scheme
                                           (Pkgtypes.License_Logic'Val (Natural'Value
                                            (USS (result (token))))));
                           when others =>
                              SU.Append (outline, result (token));
                        end case;
                     end assemble;
                  begin
                     for x in A_Token'Range loop
                        if columns (x) then
                           col_index := col_index + 1;
                           result (x) := SUS (SQLite.retrieve_string (new_stmt, col_index));
                        end if;
                     end loop;
                     if reverse_deps then
                        --  Run inner query with indefinite result set obtain the reverse
                        --  dependency information.
                        if not SQLite.reset_statement (rev_stmt) then
                           Event.emit_error ("Failed to reset reverse deps prepared stmt");
                           exit;
                        end if;
                        SQLite.bind_string (rev_stmt, 1, SQLite.retrieve_string (new_stmt, 0));
                        debug_running_stmt (rev_stmt);
                        loop
                           case SQLite.step (rev_stmt) is
                              when SQLite.row_present =>
                                 result (token_ml_rdep_namebase) :=
                                   SUS (SQLite.retrieve_string (rev_stmt, 0));
                                 result (token_ml_rdep_spkg) :=
                                   SUS (SQLite.retrieve_string (rev_stmt, 1));
                                 result (token_ml_rdep_variant) :=
                                   SUS (SQLite.retrieve_string (rev_stmt, 2));
                                 result (token_ml_rdep_version) :=
                                   SUS (SQLite.retrieve_string (rev_stmt, 3));
                                 result (token_ml_rdep_nsv) :=
                                   SUS (SQLite.retrieve_string (rev_stmt, 4));
                                 selection_tokens.Iterate (assemble'Access);
                                 Event.emit_message (USS (outline));
                                 outline := blank;

                              when SQLite.something_else =>
                                 CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                                              SQLite.get_expanded_sql (rev_stmt));
                              when SQLite.no_more_data =>
                                 exit;
                           end case;
                        end loop;
                     else
                        selection_tokens.Iterate (assemble'Access);
                        Event.emit_message (USS (outline));
                     end if;
                  end;
               when SQLite.something_else =>
                  CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                               SQLite.get_expanded_sql (new_stmt));
            end case;
         end loop;
         if reverse_deps then
            SQLite.finalize_statement (rev_stmt);
         end if;
         SQLite.finalize_statement (new_stmt);
         return True;
      end;

   end query_package_database;



end Raven.Database.UserQuery;
