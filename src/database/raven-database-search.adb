--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Event;
with Raven.Database.CommonSQL;
with Raven.Strings; use Raven.Strings;

package body Raven.Database.Search is

   -----------------------
   --  rvn_core_search  --
   -----------------------
   procedure rvn_core_search
     (db           : RDB_Connection;
      srch_pattern : String;
      behave_glob  : Boolean;
      behave_exact : Boolean;
      behave_cs    : Boolean;
      s_comment    : Boolean;
      s_desc       : Boolean;
      s_nsv        : Boolean;
      packages     : in out Pkgtypes.Package_Set.Vector)
   is
      function raw_search_field return String;
      function processed_search_field return String;
      function where_clause return String;

      func : constant String := "rvn_core_search";
      new_stmt : SQLite.thick_stmt;

      function raw_search_field return String is
      begin
         if s_comment then
            return "comment";
         elsif s_desc then
            return "desc";
         elsif s_nsv then
            return "namebase ||'-'|| subpackage ||'-'|| variant";
         else
            return "namebase";
         end if;
      end raw_search_field;

      function processed_search_field return String is
      begin
         if behave_glob or else behave_exact or else behave_cs then
            return raw_search_field & " as search_field";
         end if;
         --  default case-insensitive regex
         return "lower(" & raw_search_field & ") as search_field";
      end processed_search_field;

      function where_clause return String
      is
         sfield : constant String := " WHERE search_field ";
      begin
         if IsBlank (srch_pattern) then
            return "";
         end if;
         if behave_glob then
            return sfield & "GLOB ?1";
         elsif behave_exact then
            return sfield & "LIKE ?1";
         end if;
         --  covers regex (default), and --case-sensitive
         --  the former is handled by lower casing both the target field and the search pattern
         return sfield & "REGEXP ?1";
      end where_clause;

      sql : constant String := "SELECT " & processed_search_field & ", id" &
        ", namebase, subpackage, variant, version, abi, comment, desc, maintainer, prefix" &
        ", www, rvnsize, flatsize, licenselogic FROM packages" & where_clause;
   begin
      packages.clear;
      if not SQLite.prepare_sql (db.handle, sql, new_stmt) then
         CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func, sql);
         return;
      end if;
      if behave_glob or else behave_exact or else behave_cs then
         SQLite.bind_string (new_stmt, 1, srch_pattern);
      else
         SQLite.bind_string (new_stmt, 1, lowercase (srch_pattern));
      end if;
      debug_running_stmt (new_stmt);


      loop
         case SQLite.step (new_stmt) is
            when SQLite.row_present =>
               declare
                  pkgid : constant Pkgtypes.Package_ID :=
                                   Pkgtypes.Package_ID (SQLite.retrieve_integer (new_stmt, 1));
                  myrec : Pkgtypes.A_Package;
               begin
                  myrec.id         := pkgid;
                  myrec.namebase   := SUS (SQLite.retrieve_string (new_stmt, 2));
                  myrec.subpackage := SUS (SQLite.retrieve_string (new_stmt, 3));
                  myrec.variant    := SUS (SQLite.retrieve_string (new_stmt, 4));
                  myrec.version    := SUS (SQLite.retrieve_string (new_stmt, 5));
                  myrec.abi        := SUS (SQLite.retrieve_string (new_stmt, 6));
                  myrec.comment    := SUS (SQLite.retrieve_string (new_stmt, 7));
                  myrec.desc       := SUS (SQLite.retrieve_string (new_stmt, 8));
                  myrec.maintainer := SUS (SQLite.retrieve_string (new_stmt, 9));
                  myrec.prefix     := SUS (SQLite.retrieve_string (new_stmt, 10));
                  myrec.www        := SUS (SQLite.retrieve_string (new_stmt, 11));
                  myrec.rvnsize  := Pkgtypes.Package_Size (SQLite.retrieve_integer (new_stmt, 12));
                  myrec.flatsize := Pkgtypes.Package_Size (SQLite.retrieve_integer (new_stmt, 13));
                  myrec.licenselogic := Pkgtypes.License_Logic'Val (SQLite.retrieve_integer
                                                                    (new_stmt, 14));
                  packages.Append (myrec);
               end;
            when SQLite.something_else =>
               if not behave_glob and not behave_exact then
                  Event.emit_error
                    ("Query failed.  It's possible pattern is an illegal regular expression");
               else
                  CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                               SQLite.get_expanded_sql (new_stmt));
               end if;
               exit;
            when SQLite.no_more_data => exit;
         end case;
      end loop;
      SQLite.finalize_statement (new_stmt);

   end rvn_core_search;


   ----------------------
   --  get_categories  --
   ----------------------
   procedure print_categories
     (db     : RDB_Connection;
      prefix : String;
      pkgid  : Pkgtypes.Package_ID)
   is

      func : constant String := "get_categories";
      new_stmt : SQLite.thick_stmt;

      sql : constant String :=
        "SELECT c.name FROM pkg_categories x " &
        "JOIN categories c ON c.category_id = x.category_id " &
        "WHERE x.package_id = ?";

      nextline : Text := SU.Null_Unbounded_String;
      counter  : Natural := 0;
   begin
      if not SQLite.prepare_sql (db.handle, sql, new_stmt) then
         CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func, sql);
         return;
      end if;
      SQLite.bind_integer (new_stmt, 1, SQLite.sql_int64 (pkgid));
      debug_running_stmt (new_stmt);

      loop
         case SQLite.step (new_stmt) is
            when SQLite.row_present =>
               if counter = 0 then
                  SU.Append (nextline, SQLite.retrieve_string (new_stmt, 0));
               else
                  SU.Append (nextline, " " & SQLite.retrieve_string (new_stmt, 0));
               end if;
               counter := counter + 1;
            when SQLite.something_else =>
               CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                            SQLite.get_expanded_sql (new_stmt));
            when SQLite.no_more_data => exit;
         end case;
      end loop;
      SQLite.finalize_statement (new_stmt);

      if counter = 0 then
         return;
      end if;
      Event.emit_notice (prefix & USS (nextline));

   end print_categories;


   ----------------------
   --  print_licenses  --
   ----------------------
   procedure print_licenses
     (db     : RDB_Connection;
      prefix : String;
      pkgid  : Pkgtypes.Package_ID;
      logic  : Pkgtypes.License_Logic)
   is

      func : constant String := "get_categories";
      new_stmt : SQLite.thick_stmt;

      sql : constant String :=
        "SELECT c.name FROM pkg_licenses x " &
        "JOIN licenses c ON c.license_id = x.license_id " &
        "WHERE x.package_id = ?";

      nextline : Text := SU.Null_Unbounded_String;
      counter  : Natural := 0;
   begin
      if not SQLite.prepare_sql (db.handle, sql, new_stmt) then
         CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func, sql);
         return;
      end if;
      SQLite.bind_integer (new_stmt, 1, SQLite.sql_int64 (pkgid));
      debug_running_stmt (new_stmt);

      loop
         case SQLite.step (new_stmt) is
            when SQLite.row_present =>
               if counter = 0 then
                  SU.Append (nextline, SQLite.retrieve_string (new_stmt, 0));
               else
                  SU.Append (nextline, " " & SQLite.retrieve_string (new_stmt, 0));
               end if;
               counter := counter + 1;
            when SQLite.something_else =>
               CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                            SQLite.get_expanded_sql (new_stmt));
            when SQLite.no_more_data => exit;
         end case;
      end loop;
      SQLite.finalize_statement (new_stmt);

      if counter = 0 then
         return;
      end if;
      case logic is
         when Pkgtypes.LICENSE_UNLISTED |
              Pkgtypes.LICENSE_SINGLE =>
            Event.emit_notice (prefix & USS (nextline));
         when Pkgtypes.LICENSE_DUAL =>
            Event.emit_notice (prefix & "[dual] " & USS (nextline));
         when Pkgtypes.LICENSE_MULTI =>
            Event.emit_notice (prefix & "[multi] " & USS (nextline));
      end case;
   end print_licenses;


   -------------------------
   --  generic_multiline  --
   -------------------------
   procedure generic_multiline
     (db     : RDB_Connection;
      pkgid  : Pkgtypes.Package_ID;
      func   : String;
      prefix : String;
      sql    : String)
   is
      function make_blank_prefix return String is
      begin
         if prefix'length = 0 then
            return "";
         end if;
         declare
            canvas : String (1 .. prefix'Length) := (others => ' ');
         begin
            canvas (canvas'Last - 1) := ':';
            return canvas;
         end;
      end make_blank_prefix;

      blank_prefix : constant String := make_blank_prefix;
      new_stmt : SQLite.thick_stmt;
      counter  : Natural := 0;
   begin
      if not SQLite.prepare_sql (db.handle, sql, new_stmt) then
         CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func, sql);
         return;
      end if;
      SQLite.bind_integer (new_stmt, 1, SQLite.sql_int64 (pkgid));
      debug_running_stmt (new_stmt);

      loop
         case SQLite.step (new_stmt) is
            when SQLite.row_present =>
               if counter = 0 then
                  Event.emit_notice (prefix & SQLite.retrieve_string (new_stmt, 0));
               else
                  Event.emit_notice (blank_prefix & SQLite.retrieve_string (new_stmt, 0));
               end if;
               counter := counter + 1;
            when SQLite.something_else =>
               CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                            SQLite.get_expanded_sql (new_stmt));
            when SQLite.no_more_data => exit;
         end case;
      end loop;
      SQLite.finalize_statement (new_stmt);
   end generic_multiline;


   -----------------------------
   --  generic_multiline_map  --
   -----------------------------
   procedure generic_multiline_map
     (db     : RDB_Connection;
      pkgid  : Pkgtypes.Package_ID;
      func   : String;
      prefix : String;
      sql    : String)
   is
      function make_blank_prefix return String is
      begin
         if prefix'length = 0 then
            return "";
         end if;
         declare
            canvas : String (1 .. prefix'Length) := (others => ' ');
         begin
            canvas (canvas'Last - 1) := ':';
            return canvas;
         end;
      end make_blank_prefix;

      blank_prefix : constant String := make_blank_prefix;
      new_stmt : SQLite.thick_stmt;
      counter  : Natural := 0;
   begin
      if not SQLite.prepare_sql (db.handle, sql, new_stmt) then
         CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func, sql);
         return;
      end if;
      SQLite.bind_integer (new_stmt, 1, SQLite.sql_int64 (pkgid));
      debug_running_stmt (new_stmt);

      loop
         case SQLite.step (new_stmt) is
            when SQLite.row_present =>
               declare
                  payload : constant String := SQLite.retrieve_string (new_stmt, 0) &
                    " => " & SQLite.retrieve_string (new_stmt, 1);
               begin
                  if counter = 0 then
                     Event.emit_notice (prefix & payload);
                  else
                     Event.emit_notice (blank_prefix & payload);
                  end if;
                  counter := counter + 1;
               end;
            when SQLite.something_else =>
               CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                            SQLite.get_expanded_sql (new_stmt));
            when SQLite.no_more_data => exit;
         end case;
      end loop;
      SQLite.finalize_statement (new_stmt);
   end generic_multiline_map;


   --------------------------------
   --  print_libraries_required  --
   --------------------------------
   procedure print_libraries_required
     (db     : RDB_Connection;
      prefix : String;
      pkgid  : Pkgtypes.Package_ID)
   is
      func : constant String := "print_libraries_required";
      sql : constant String :=
        "SELECT c.name FROM pkg_libs_required x " &
        "JOIN libraries c ON c.library_id = x.library_id " &
        "WHERE x.package_id = ? " &
        "ORDER BY c.name";
   begin
      generic_multiline (db, pkgid, func, prefix, sql);
   end print_libraries_required;


   --------------------------------
   --  print_libraries_provided  --
   --------------------------------
   procedure print_libraries_provided
     (db     : RDB_Connection;
      prefix : String;
      pkgid  : Pkgtypes.Package_ID)
   is
      func : constant String := "print_libraries_provided";
      sql : constant String :=
        "SELECT c.name FROM pkg_libs_provided x " &
        "JOIN libraries c ON c.library_id = x.library_id " &
        "WHERE x.package_id = ? " &
        "ORDER BY c.name";
   begin
      generic_multiline (db, pkgid, func, prefix, sql);
   end print_libraries_provided;


   --------------------------
   --  print_dependencies  --
   --------------------------
   procedure print_dependencies
     (db     : RDB_Connection;
      prefix : String;
      pkgid  : Pkgtypes.Package_ID)
   is
      func : constant String := "print_dependencies";
      sql : constant String :=
        "SELECT d.nsv ||'-'|| d.version as nsvv " &
        "FROM pkg_dependencies x " &
        "JOIN dependencies d on x.dependency_id = d.dependency_id " &
        "WHERE x.package_id = ? " &
        "ORDER BY nsvv";
   begin
      generic_multiline (db, pkgid, func, prefix, sql);
   end print_dependencies;


   ----------------------------------
   --  print_reverse_dependencies  --
   ----------------------------------
   procedure print_reverse_dependencies
     (db     : RDB_Connection;
      prefix : String;
      pkgid  : Pkgtypes.Package_ID)
   is
      func : constant String := "print_reverse_dependencies";
      subquery : constant String := "(SELECT d.dependency_id FROM dependencies d WHERE nsv = " &
        "(SELECT namebase ||'-'|| subpackage ||'-'|| variant FROM packages WHERE id = ?)) ";
      sql : constant String :=
        "SELECT p.namebase ||'-'|| p.subpackage ||'-'|| p.variant ||'-'|| p.version as nsvv " &
        "FROM packages as p " &
        "JOIN pkg_dependencies x on x.package_id = p.id " &
        "WHERE x.dependency_id = " & subquery &
        "ORDER by nsvv";
   begin
      generic_multiline (db, pkgid, func, prefix, sql);
   end print_reverse_dependencies;


   -------------------------
   --  print_annotations  --
   -------------------------
   procedure print_annotations
     (db     : RDB_Connection;
      prefix : String;
      pkgid  : Pkgtypes.Package_ID)
   is
      func : constant String := "print_annotations";
      sql : constant String :=
        "SELECT ml.note_key, x.annotation " &
        "FROM packages p " &
        "JOIN pkg_annotations x on x.package_id = p.id " &
        "JOIN annotations ml on x.annotation_id = ml.annotation_id " &
        "WHERE p.id = ? " &
        "ORDER BY ml.note_key";
   begin
       generic_multiline_map (db, pkgid, func, prefix, sql);
   end print_annotations;


   ---------------------
   --  print_options  --
   ---------------------
   procedure print_options
     (db     : RDB_Connection;
      prefix : String;
      pkgid  : Pkgtypes.Package_ID)
   is
      func : constant String := "print_annotations";
      sql : constant String :=
        "SELECT ml.option_name, x.option_setting " &
        "FROM packages p " &
        "JOIN pkg_options x on x.package_id = p.id " &
        "JOIN options ml on x.option_id = ml.option_id " &
        "WHERE p.id = ? " &
        "ORDER BY ml.option_name";
   begin
       generic_multiline_map (db, pkgid, func, prefix, sql);
   end print_options;

end Raven.Database.Search;
