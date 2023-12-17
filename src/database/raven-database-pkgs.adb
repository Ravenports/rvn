--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Event;
with Raven.Strings;
with Raven.Database.CommonSQL;
with Raven.Database.Operations;
with Archive.Whitelist;

use Raven.Strings;

package body Raven.Database.Pkgs is

   package OPS renames Raven.Database.Operations;

   ----------------------------
   --  rdb_register_package  --
   ----------------------------
   function rdb_register_package
     (db  : in out RDB_Connection;
      pkg : in out Pkgtypes.A_Package;
      forced : Boolean) return Boolean
   is
      save : constant String := "REGPACK";
      func : constant String := "rdb_register_package";
      onward : Boolean;
   begin
      if not CommonSQL.transaction_begin (db.handle, internal_srcfile, func, save) then
         Event.emit_error (func & ": Failed to start transaction");
         return False;
      end if;
      onward := run_prstmt_main_pkg (db, pkg);
      if onward then
         pkg.id := Pkgtypes.Package_ID (SQLite.get_last_insert_rowid (db.handle));
         -- TODO Update dep information on packages that depend on the inserted package
      end if;

      if onward then
         onward := run_prstmt_category (db, pkg);
      end if;
      if onward then
         onward := run_prstmt_license (db, pkg);
      end if;
      if onward then
         onward := run_prstmt_user (db, pkg);
      end if;
      if onward then
         onward := run_prstmt_group (db, pkg);
      end if;
      if onward then
         onward := run_prstmt_directory (db, pkg);
      end if;
      if onward then
         onward := run_prstmt_script (db, pkg);
      end if;
      if onward then
         onward := run_prstmt_library (db, pkg);
      end if;
      if onward then
         onward := run_prstmt_note (db, pkg);
      end if;
      if onward then
         onward := run_prstmt_option (db, pkg);
      end if;
      if onward then
         onward := run_prstmt_depend (db, pkg);
      end if;

      if onward then
         if not CommonSQL.transaction_commit (db.handle, internal_srcfile, func, save) then
            Event.emit_error (func & ": Failed to commit transaction");
            return False;
         end if;
         return True;
      else
         if not CommonSQL.transaction_rollback (db.handle, internal_srcfile, func, save) then
            Event.emit_error (func & ": Failed to rollback transaction");
         end if;
         return False;
      end if;
   end rdb_register_package;


   --------------------------
   --  squawk_reset_error  --
   --------------------------
   procedure squawk_reset_error (stmt_type : SCH.prepared_statement) is
   begin
      Event.emit_error ("failed to reset " & stmt_type'Img & " prepared statement");
   end squawk_reset_error;


   --------------------------
   --  debug_running_stmt  --
   --------------------------
   procedure debug_running_stmt (stmt : SQLite.thick_stmt) is
   begin
      Event.emit_debug (high_level, "[rdb] running: " & SQLite.get_expanded_sql (stmt));
   end debug_running_stmt;


   ---------------------------
   --  run_prstmt_main_pkg  --
   ---------------------------
   function run_prstmt_main_pkg (db : RDB_Connection; pkg : Pkgtypes.A_Package) return Boolean
   is
      this_stmt : SQLite.thick_stmt renames OPS.prepared_statements (SCH.main_pkg);
      automatic : SQLite.sql_int64 := 0;
      liclogic  : SQLite.sql_int64;
      func      : constant String := "run_prstmt_main_pkg";
   begin
      if pkg.automatic then
         automatic := 1;
      end if;
      case pkg.licenselogic is
         when Pkgtypes.LICENSE_UNLISTED => liclogic := 0;
         when Pkgtypes.LICENSE_SINGLE   => liclogic := 1;
         when Pkgtypes.LICENSE_DUAL     => liclogic := 2;
         when Pkgtypes.LICENSE_MULTI    => liclogic := 3;
      end case;

      if not SQLite.reset_statement (this_stmt) then
         squawk_reset_error (SCH.main_pkg);
         return False;
      end if;
      SQLite.bind_string (this_stmt, 1, USS (pkg.namebase));
      SQLite.bind_string (this_stmt, 2, USS (pkg.subpackage));
      SQLite.bind_string (this_stmt, 3, USS (pkg.variant));
      SQLite.bind_string (this_stmt, 4, USS (pkg.version));
      SQLite.bind_string (this_stmt, 5, USS (pkg.comment));
      SQLite.bind_string (this_stmt, 6, USS (pkg.desc));
      SQLite.bind_string (this_stmt, 7, USS (pkg.www));
      SQLite.bind_string (this_stmt, 8, USS (pkg.maintainer));
      SQLite.bind_string (this_stmt, 9, USS (pkg.prefix));
      SQLite.bind_string (this_stmt, 10, USS (pkg.abi));
      SQLite.bind_integer (this_stmt, 11, SQLite.sql_int64 (pkg.flatsize));
      SQLite.bind_integer (this_stmt, 12, liclogic);
      SQLite.bind_integer (this_stmt, 13, automatic);

      debug_running_stmt (this_stmt);

      case SQLite.step (this_stmt) is
         when SQLite.no_more_data => return True;
         when SQLite.row_present | SQLite.something_else =>
            CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                         SQLite.get_expanded_sql (this_stmt));
            return False;
      end case;

   end run_prstmt_main_pkg;


   ---------------------------
   --  run_prstmt_category  --
   ---------------------------
   function run_prstmt_category (db : RDB_Connection; pkg : Pkgtypes.A_Package) return Boolean
   is
      main_stmt : SQLite.thick_stmt renames OPS.prepared_statements (SCH.category);
      pack_stmt : SQLite.thick_stmt renames OPS.prepared_statements (SCH.pkg_category);

      func       : constant String := "run_prstmt_category";
      keep_going : Boolean := True;

      procedure insert_main (Position : Pkgtypes.Text_List.Cursor)
      is
         category : constant String := USS (Pkgtypes.Text_List.Element (Position));
      begin
         if keep_going then
            SQLite.bind_string (main_stmt, 1, category);
            debug_running_stmt (main_stmt);
            case SQLite.step (main_stmt) is
               when SQLite.no_more_data => null;
               when SQLite.row_present | SQLite.something_else =>
                  CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                         SQLite.get_expanded_sql (main_stmt));
                  keep_going := False;
            end case;
         end if;
      end insert_main;

      procedure insert_into_package (Position : Pkgtypes.Text_List.Cursor)
      is
         category : constant String := USS (Pkgtypes.Text_List.Element (Position));
      begin
         if keep_going then
            SQLite.bind_integer (pack_stmt, 1, SQLite.sql_int64 (pkg.id));
            SQLite.bind_string  (pack_stmt, 2, category);
            debug_running_stmt (pack_stmt);
            case SQLite.step (pack_stmt) is
               when SQLite.no_more_data => null;
               when SQLite.row_present | SQLite.something_else =>
                  CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                         SQLite.get_expanded_sql (pack_stmt));
                  keep_going := False;
            end case;
         end if;
      end insert_into_package;
   begin
      if not SQLite.reset_statement (main_stmt) then
         squawk_reset_error (SCH.category);
         return False;
      end if;
      pkg.categories.Iterate (insert_main'Access);

      if keep_going then
         if not SQLite.reset_statement (pack_stmt) then
            squawk_reset_error (SCH.pkg_category);
            return False;
         end if;
         pkg.categories.Iterate (insert_into_package'Access);
      end if;

      return keep_going;
   end run_prstmt_category;


   --------------------------
   --  run_prstmt_license  --
   --------------------------
   function run_prstmt_license (db : RDB_Connection; pkg : Pkgtypes.A_Package) return Boolean
   is
      main_stmt : SQLite.thick_stmt renames OPS.prepared_statements (SCH.license);
      pack_stmt : SQLite.thick_stmt renames OPS.prepared_statements (SCH.pkg_license);

      func       : constant String := "run_prstmt_license";
      keep_going : Boolean := True;

      procedure insert_main (Position : Pkgtypes.Text_List.Cursor)
      is
         license : constant String := USS (Pkgtypes.Text_List.Element (Position));
      begin
         if keep_going then
            SQLite.bind_string (main_stmt, 1, license);
            debug_running_stmt (main_stmt);
            case SQLite.step (main_stmt) is
               when SQLite.no_more_data => null;
               when SQLite.row_present | SQLite.something_else =>
                  CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                         SQLite.get_expanded_sql (main_stmt));
                  keep_going := False;
            end case;
         end if;
      end insert_main;

      procedure insert_into_package (Position : Pkgtypes.Text_List.Cursor)
      is
         license : constant String := USS (Pkgtypes.Text_List.Element (Position));
      begin
         if keep_going then
            SQLite.bind_integer (pack_stmt, 1, SQLite.sql_int64 (pkg.id));
            SQLite.bind_string  (pack_stmt, 2, license);
            debug_running_stmt (pack_stmt);
            case SQLite.step (pack_stmt) is
               when SQLite.no_more_data => null;
               when SQLite.row_present | SQLite.something_else =>
                  CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                         SQLite.get_expanded_sql (pack_stmt));
                  keep_going := False;
            end case;
         end if;
      end insert_into_package;
   begin
      if not SQLite.reset_statement (main_stmt) then
         squawk_reset_error (SCH.license);
         return False;
      end if;
      pkg.licenses.Iterate (insert_main'Access);

      if keep_going then
         if not SQLite.reset_statement (pack_stmt) then
            squawk_reset_error (SCH.pkg_license);
            return False;
         end if;
         pkg.licenses.Iterate (insert_into_package'Access);
      end if;

      return keep_going;
   end run_prstmt_license;


   -----------------------
   --  run_prstmt_user  --
   -----------------------
   function run_prstmt_user (db : RDB_Connection; pkg : Pkgtypes.A_Package) return Boolean
   is
      main_stmt : SQLite.thick_stmt renames OPS.prepared_statements (SCH.user);
      pack_stmt : SQLite.thick_stmt renames OPS.prepared_statements (SCH.pkg_user);

      func       : constant String := "run_prstmt_user";
      keep_going : Boolean := True;

      procedure insert_main (Position : Pkgtypes.Text_List.Cursor)
      is
         user : constant String := USS (Pkgtypes.Text_List.Element (Position));
      begin
         if keep_going then
            SQLite.bind_string (main_stmt, 1, user);
            debug_running_stmt (main_stmt);
            case SQLite.step (main_stmt) is
               when SQLite.no_more_data => null;
               when SQLite.row_present | SQLite.something_else =>
                  CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                         SQLite.get_expanded_sql (main_stmt));
                  keep_going := False;
            end case;
         end if;
      end insert_main;

      procedure insert_into_package (Position : Pkgtypes.Text_List.Cursor)
      is
         user : constant String := USS (Pkgtypes.Text_List.Element (Position));
      begin
         if keep_going then
            SQLite.bind_integer (pack_stmt, 1, SQLite.sql_int64 (pkg.id));
            SQLite.bind_string  (pack_stmt, 2, user);
            debug_running_stmt (pack_stmt);
            case SQLite.step (pack_stmt) is
               when SQLite.no_more_data => null;
               when SQLite.row_present | SQLite.something_else =>
                  CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                         SQLite.get_expanded_sql (pack_stmt));
                  keep_going := False;
            end case;
         end if;
      end insert_into_package;
   begin
      if not SQLite.reset_statement (main_stmt) then
         squawk_reset_error (SCH.user);
         return False;
      end if;
      pkg.users.Iterate (insert_main'Access);

      if keep_going then
         if not SQLite.reset_statement (pack_stmt) then
            squawk_reset_error (SCH.pkg_user);
            return False;
         end if;
         pkg.users.Iterate (insert_into_package'Access);
      end if;

      return keep_going;
   end run_prstmt_user;


   ------------------------
   --  run_prstmt_group  --
   ------------------------
   function run_prstmt_group (db : RDB_Connection; pkg : Pkgtypes.A_Package) return Boolean
   is
      main_stmt : SQLite.thick_stmt renames OPS.prepared_statements (SCH.group);
      pack_stmt : SQLite.thick_stmt renames OPS.prepared_statements (SCH.pkg_group);

      func       : constant String := "run_prstmt_group";
      keep_going : Boolean := True;

      procedure insert_main (Position : Pkgtypes.Text_List.Cursor)
      is
         group : constant String := USS (Pkgtypes.Text_List.Element (Position));
      begin
         if keep_going then
            SQLite.bind_string (main_stmt, 1, group);
            debug_running_stmt (main_stmt);
            case SQLite.step (main_stmt) is
               when SQLite.no_more_data => null;
               when SQLite.row_present | SQLite.something_else =>
                  CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                         SQLite.get_expanded_sql (main_stmt));
                  keep_going := False;
            end case;
         end if;
      end insert_main;

      procedure insert_into_package (Position : Pkgtypes.Text_List.Cursor)
      is
         group : constant String := USS (Pkgtypes.Text_List.Element (Position));
      begin
         if keep_going then
            SQLite.bind_integer (pack_stmt, 1, SQLite.sql_int64 (pkg.id));
            SQLite.bind_string  (pack_stmt, 2, group);
            debug_running_stmt (pack_stmt);
            case SQLite.step (pack_stmt) is
               when SQLite.no_more_data => null;
               when SQLite.row_present | SQLite.something_else =>
                  CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                         SQLite.get_expanded_sql (pack_stmt));
                  keep_going := False;
            end case;
         end if;
      end insert_into_package;
   begin
      if not SQLite.reset_statement (main_stmt) then
         squawk_reset_error (SCH.group);
         return False;
      end if;
      pkg.groups.Iterate (insert_main'Access);

      if keep_going then
         if not SQLite.reset_statement (pack_stmt) then
            squawk_reset_error (SCH.pkg_group);
            return False;
         end if;
         pkg.groups.Iterate (insert_into_package'Access);
      end if;

      return keep_going;
   end run_prstmt_group;


   ----------------------------
   --  run_prstmt_directory  --
   ----------------------------
   function run_prstmt_directory (db : RDB_Connection; pkg : Pkgtypes.A_Package) return Boolean
   is
      main_stmt : SQLite.thick_stmt renames OPS.prepared_statements (SCH.directory);
      pack_stmt : SQLite.thick_stmt renames OPS.prepared_statements (SCH.pkg_directory);

      func       : constant String := "run_prstmt_directory";
      keep_going : Boolean := True;

      procedure insert_main (Position : Pkgtypes.Text_List.Cursor)
      is
         directory : constant String := USS (Pkgtypes.Text_List.Element (Position));
      begin
         if keep_going then
            SQLite.bind_string (main_stmt, 1, directory);
            debug_running_stmt (main_stmt);
            case SQLite.step (main_stmt) is
               when SQLite.no_more_data => null;
               when SQLite.row_present | SQLite.something_else =>
                  CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                         SQLite.get_expanded_sql (main_stmt));
                  keep_going := False;
            end case;
         end if;
      end insert_main;

      procedure insert_into_package (Position : Pkgtypes.Text_List.Cursor)
      is
         directory : constant String := USS (Pkgtypes.Text_List.Element (Position));
      begin
         if keep_going then
            SQLite.bind_integer (pack_stmt, 1, SQLite.sql_int64 (pkg.id));
            SQLite.bind_string  (pack_stmt, 2, directory);
            debug_running_stmt (pack_stmt);
            case SQLite.step (pack_stmt) is
               when SQLite.no_more_data => null;
               when SQLite.row_present | SQLite.something_else =>
                  CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                         SQLite.get_expanded_sql (pack_stmt));
                  keep_going := False;
            end case;
         end if;
      end insert_into_package;
   begin
      if not SQLite.reset_statement (main_stmt) then
         squawk_reset_error (SCH.directory);
         return False;
      end if;
      pkg.directories.Iterate (insert_main'Access);

      if keep_going then
         if not SQLite.reset_statement (pack_stmt) then
            squawk_reset_error (SCH.pkg_directory);
            return False;
         end if;
         pkg.directories.Iterate (insert_into_package'Access);
      end if;

      return keep_going;
   end run_prstmt_directory;


   -------------------------
   --  run_prstmt_script  --
   -------------------------
   function run_prstmt_script (db : RDB_Connection; pkg : Pkgtypes.A_Package) return Boolean
   is
      main_stmt : SQLite.thick_stmt renames OPS.prepared_statements (SCH.script);
      pack_stmt : SQLite.thick_stmt renames OPS.prepared_statements (SCH.pkg_script);

      func       : constant String := "run_prstmt_script";
      keep_going : Boolean := True;
      phase_type : Natural := 0;
      phase_ndx  : Natural := 0;

      procedure insert_main (Position : Pkgtypes.Script_List.Cursor)
      is
         code : constant String := USS (Pkgtypes.Script_List.Element (Position).code);
      begin
         if keep_going then
            SQLite.bind_string (main_stmt, 1, code);
            debug_running_stmt (main_stmt);
            case SQLite.step (main_stmt) is
               when SQLite.no_more_data => null;
               when SQLite.row_present | SQLite.something_else =>
                  CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                         SQLite.get_expanded_sql (main_stmt));
                  keep_going := False;
            end case;
         end if;
      end insert_main;

      procedure insert_into_package (Position : Pkgtypes.Script_List.Cursor)
      is
         --  package_id,script_type,type_index,arguments,(code)
         code : constant String := USS (Pkgtypes.Script_List.Element (Position).code);
         args : constant String := USS (Pkgtypes.Script_List.Element (Position).args);
      begin
         if keep_going then
            SQLite.bind_integer (pack_stmt, 1, SQLite.sql_int64 (pkg.id));
            SQLite.bind_integer (pack_stmt, 2, SQLite.sql_int64 (phase_type));
            SQLite.bind_integer (pack_stmt, 3, SQLite.sql_int64 (phase_ndx));
            SQLite.bind_string  (pack_stmt, 4, args);
            SQLite.bind_string  (pack_stmt, 5, code);
            debug_running_stmt (pack_stmt);
            case SQLite.step (pack_stmt) is
               when SQLite.no_more_data => null;
               when SQLite.row_present | SQLite.something_else =>
                  CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                         SQLite.get_expanded_sql (pack_stmt));
                  keep_going := False;
            end case;
            phase_ndx := phase_ndx + 1;
         end if;
      end insert_into_package;
   begin
      if not SQLite.reset_statement (main_stmt) then
         squawk_reset_error (SCH.script);
         return False;
      end if;
      for phase in Archive.Whitelist.package_phase'Range loop
         pkg.scripts (phase).Iterate (insert_main'Access);
      end loop;

      if keep_going then
         if not SQLite.reset_statement (pack_stmt) then
            squawk_reset_error (SCH.pkg_script);
            return False;
         end if;
         for phase in Archive.Whitelist.package_phase'Range loop
            phase_type := phase_type + 1;  --  type index starts at 1
            phase_ndx := 0;                --  script array starts at 0

            pkg.scripts (phase).Iterate (insert_into_package'Access);
         end loop;
      end if;

      return keep_going;
   end run_prstmt_script;


   --------------------------
   --  run_prstmt_library  --
   --------------------------
   function run_prstmt_library (db : RDB_Connection; pkg : Pkgtypes.A_Package) return Boolean
   is
      main_stmt  : SQLite.thick_stmt renames OPS.prepared_statements (SCH.library);
      pack_stmt1 : SQLite.thick_stmt renames OPS.prepared_statements (SCH.pkg_adjacent_lib);
      pack_stmt2 : SQLite.thick_stmt renames OPS.prepared_statements (SCH.pkg_provided_lib);
      pack_stmt3 : SQLite.thick_stmt renames OPS.prepared_statements (SCH.pkg_required_lib);

      func       : constant String := "run_prstmt_library";
      keep_going : Boolean := True;

      procedure insert_main (Position : Pkgtypes.Text_List.Cursor)
      is
         library : constant String := USS (Pkgtypes.Text_List.Element (Position));
      begin
         if keep_going then
            SQLite.bind_string (main_stmt, 1, library);
            debug_running_stmt (main_stmt);
            case SQLite.step (main_stmt) is
               when SQLite.no_more_data => null;
               when SQLite.row_present | SQLite.something_else =>
                  CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                         SQLite.get_expanded_sql (main_stmt));
                  keep_going := False;
            end case;
         end if;
      end insert_main;

      procedure insert_into_package1 (Position : Pkgtypes.Text_List.Cursor)
      is
         license : constant String := USS (Pkgtypes.Text_List.Element (Position));
      begin
         if keep_going then
            SQLite.bind_integer (pack_stmt1, 1, SQLite.sql_int64 (pkg.id));
            SQLite.bind_string  (pack_stmt1, 2, license);
            debug_running_stmt (pack_stmt1);
            case SQLite.step (pack_stmt1) is
               when SQLite.no_more_data => null;
               when SQLite.row_present | SQLite.something_else =>
                  CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                         SQLite.get_expanded_sql (pack_stmt1));
                  keep_going := False;
            end case;
         end if;
      end insert_into_package1;

      procedure insert_into_package2 (Position : Pkgtypes.Text_List.Cursor)
      is
         license : constant String := USS (Pkgtypes.Text_List.Element (Position));
      begin
         if keep_going then
            SQLite.bind_integer (pack_stmt2, 1, SQLite.sql_int64 (pkg.id));
            SQLite.bind_string  (pack_stmt2, 2, license);
            debug_running_stmt (pack_stmt2);
            case SQLite.step (pack_stmt2) is
               when SQLite.no_more_data => null;
               when SQLite.row_present | SQLite.something_else =>
                  CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                         SQLite.get_expanded_sql (pack_stmt2));
                  keep_going := False;
            end case;
         end if;
      end insert_into_package2;

      procedure insert_into_package3 (Position : Pkgtypes.Text_List.Cursor)
      is
         license : constant String := USS (Pkgtypes.Text_List.Element (Position));
      begin
         if keep_going then
            SQLite.bind_integer (pack_stmt3, 1, SQLite.sql_int64 (pkg.id));
            SQLite.bind_string  (pack_stmt3, 2, license);
            debug_running_stmt (pack_stmt3);
            case SQLite.step (pack_stmt3) is
               when SQLite.no_more_data => null;
               when SQLite.row_present | SQLite.something_else =>
                  CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                         SQLite.get_expanded_sql (pack_stmt3));
                  keep_going := False;
            end case;
         end if;
      end insert_into_package3;
   begin
      if not SQLite.reset_statement (main_stmt) then
         squawk_reset_error (SCH.library);
         return False;
      end if;

      pkg.libs_adjacent.Iterate (insert_main'Access);
      if keep_going then
         pkg.libs_provided.Iterate (insert_main'Access);
      end if;
      if keep_going then
         pkg.libs_required.Iterate (insert_main'Access);
      end if;

      if keep_going then
         if not SQLite.reset_statement (pack_stmt1) then
            squawk_reset_error (SCH.pkg_adjacent_lib);
            return False;
         end if;
         if not SQLite.reset_statement (pack_stmt2) then
            squawk_reset_error (SCH.pkg_provided_lib);
            return False;
         end if;
         if not SQLite.reset_statement (pack_stmt2) then
            squawk_reset_error (SCH.pkg_required_lib);
            return False;
         end if;
         pkg.libs_adjacent.Iterate (insert_into_package1'Access);
      end if;

      if keep_going then
         pkg.libs_provided.Iterate (insert_into_package2'Access);
      end if;
      if keep_going then
         pkg.libs_required.Iterate (insert_into_package3'Access);
      end if;

      return keep_going;
   end run_prstmt_library;


   -----------------------
   --  run_prstmt_note  --
   -----------------------
   function run_prstmt_note (db : RDB_Connection; pkg : Pkgtypes.A_Package) return Boolean
   is
      main_stmt : SQLite.thick_stmt renames OPS.prepared_statements (SCH.note);
      pack_stmt : SQLite.thick_stmt renames OPS.prepared_statements (SCH.pkg_note);

      func       : constant String := "run_prstmt_note";
      keep_going : Boolean := True;

      procedure insert_main (Position : Pkgtypes.NV_Pairs.Cursor)
      is
         note_key : constant String := USS (Pkgtypes.NV_Pairs.Key (Position));
      begin
         if keep_going then
            SQLite.bind_string (main_stmt, 1, note_key);
            debug_running_stmt (main_stmt);
            case SQLite.step (main_stmt) is
               when SQLite.no_more_data => null;
               when SQLite.row_present | SQLite.something_else =>
                  CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                         SQLite.get_expanded_sql (main_stmt));
                  keep_going := False;
            end case;
         end if;
      end insert_main;

      procedure insert_into_package (Position : Pkgtypes.NV_Pairs.Cursor)
      is
         note_key  : constant String := USS (Pkgtypes.NV_Pairs.Key (Position));
         note_text : constant String := USS (Pkgtypes.NV_Pairs.Element (Position));
      begin
         if keep_going then
            SQLite.bind_integer (pack_stmt, 1, SQLite.sql_int64 (pkg.id));
            SQLite.bind_string  (pack_stmt, 2, note_key);
            SQLite.bind_string  (pack_stmt, 3, note_text);
            debug_running_stmt (pack_stmt);
            case SQLite.step (pack_stmt) is
               when SQLite.no_more_data => null;
               when SQLite.row_present | SQLite.something_else =>
                  CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                         SQLite.get_expanded_sql (pack_stmt));
                  keep_going := False;
            end case;
         end if;
      end insert_into_package;
   begin
      if not SQLite.reset_statement (main_stmt) then
         squawk_reset_error (SCH.note);
         return False;
      end if;
      pkg.annotations.Iterate (insert_main'Access);

      if keep_going then
         if not SQLite.reset_statement (pack_stmt) then
            squawk_reset_error (SCH.pkg_note);
            return False;
         end if;
         pkg.annotations.Iterate (insert_into_package'Access);
      end if;

      return keep_going;
   end run_prstmt_note;


   -------------------------
   --  run_prstmt_option  --
   -------------------------
   function run_prstmt_option (db : RDB_Connection; pkg : Pkgtypes.A_Package) return Boolean
   is
      main_stmt : SQLite.thick_stmt renames OPS.prepared_statements (SCH.option);
      pack_stmt : SQLite.thick_stmt renames OPS.prepared_statements (SCH.pkg_option);

      func       : constant String := "run_prstmt_option";
      keep_going : Boolean := True;

      procedure insert_main (Position : Pkgtypes.NV_Pairs.Cursor)
      is
         opt_name : constant String := USS (Pkgtypes.NV_Pairs.Key (Position));
      begin
         if keep_going then
            SQLite.bind_string (main_stmt, 1, opt_name);
            debug_running_stmt (main_stmt);
            case SQLite.step (main_stmt) is
               when SQLite.no_more_data => null;
               when SQLite.row_present | SQLite.something_else =>
                  CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                         SQLite.get_expanded_sql (main_stmt));
                  keep_going := False;
            end case;
         end if;
      end insert_main;

      procedure insert_into_package (Position : Pkgtypes.NV_Pairs.Cursor)
      is
         opt_name  : constant String := USS (Pkgtypes.NV_Pairs.Key (Position));
         opt_value : constant String := USS (Pkgtypes.NV_Pairs.Element (Position));
      begin
         if keep_going then
            SQLite.bind_integer (pack_stmt, 1, SQLite.sql_int64 (pkg.id));
            SQLite.bind_string  (pack_stmt, 2, opt_name);
            SQLite.bind_string  (pack_stmt, 3, opt_value);
            debug_running_stmt (pack_stmt);
            case SQLite.step (pack_stmt) is
               when SQLite.no_more_data => null;
               when SQLite.row_present | SQLite.something_else =>
                  CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                         SQLite.get_expanded_sql (pack_stmt));
                  keep_going := False;
            end case;
         end if;
      end insert_into_package;
   begin
      if not SQLite.reset_statement (main_stmt) then
         squawk_reset_error (SCH.option);
         return False;
      end if;
      pkg.options.Iterate (insert_main'Access);

      if keep_going then
         if not SQLite.reset_statement (pack_stmt) then
            squawk_reset_error (SCH.pkg_option);
            return False;
         end if;
         pkg.options.Iterate (insert_into_package'Access);
      end if;

      return keep_going;
   end run_prstmt_option;


   -------------------------
   --  run_prstmt_depend  --
   -------------------------
   function run_prstmt_depend (db : RDB_Connection; pkg : Pkgtypes.A_Package) return Boolean
   is
      main_stmt : SQLite.thick_stmt renames OPS.prepared_statements (SCH.dependency);
      pack_stmt : SQLite.thick_stmt renames OPS.prepared_statements (SCH.pkg_dependency);

      func       : constant String := "run_prstmt_depend";
      keep_going : Boolean := True;

      procedure insert_main (Position : Pkgtypes.Text_List.Cursor)
      is
         fulldep : constant String := USS (Pkgtypes.Text_List.Element (Position));
      begin
         if keep_going then
            SQLite.bind_string (main_stmt, 1, head (fulldep, "-"));
            SQLite.bind_string (main_stmt, 2, tail (fulldep, "-"));
            debug_running_stmt (main_stmt);
            case SQLite.step (main_stmt) is
               when SQLite.no_more_data => null;
               when SQLite.row_present | SQLite.something_else =>
                  CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                         SQLite.get_expanded_sql (main_stmt));
                  keep_going := False;
            end case;
         end if;
      end insert_main;

      procedure insert_into_package (Position : Pkgtypes.Text_List.Cursor)
      is
         fulldep : constant String := USS (Pkgtypes.Text_List.Element (Position));
      begin
         if keep_going then
            SQLite.bind_integer (pack_stmt, 1, SQLite.sql_int64 (pkg.id));
            SQLite.bind_string  (pack_stmt, 2, head (fulldep, "-"));
            SQLite.bind_string  (pack_stmt, 3, tail (fulldep, "-"));
            debug_running_stmt (pack_stmt);
            case SQLite.step (pack_stmt) is
               when SQLite.no_more_data => null;
               when SQLite.row_present | SQLite.something_else =>
                  CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                         SQLite.get_expanded_sql (pack_stmt));
                  keep_going := False;
            end case;
         end if;
      end insert_into_package;
   begin
      if not SQLite.reset_statement (main_stmt) then
         squawk_reset_error (SCH.dependency);
         return False;
      end if;
      pkg.dependencies.Iterate (insert_main'Access);

      if keep_going then
         if not SQLite.reset_statement (pack_stmt) then
            squawk_reset_error (SCH.pkg_dependency);
            return False;
         end if;
         pkg.dependencies.Iterate (insert_into_package'Access);
      end if;

      return keep_going;
   end run_prstmt_depend;


end Raven.Database.Pkgs;
