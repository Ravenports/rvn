--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Directories;
with Ada.Characters.Latin_1;
with Raven.Event;
with Raven.Strings;
with Raven.Context;
with Raven.Database.CommonSQL;
with Raven.Database.Operations;
with Raven.Database.Query;
with Raven.Cmd.Unset;
with Archive.Whitelist;

use Raven.Strings;

package body Raven.Database.Pkgs is

   package LAT renames Ada.Characters.Latin_1;
   package OPS renames Raven.Database.Operations;
   package QRY renames Raven.Database.Query;
   package RCU renames Raven.Cmd.Unset;


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
      onward : Boolean := True;
      pkgid  : Pkgtypes.Package_ID := Pkgtypes.Package_Not_Installed;
      old_files : Archive.Unpack.file_records.Vector;
      new_package : Boolean := True;
   begin
      if not CommonSQL.transaction_begin (db.handle, internal_srcfile, func, save) then
         Event.emit_error (func & ": Failed to start transaction");
         return False;
      end if;

      if forced then
         pkgid := QRY.package_installed (db, USS (pkg.namebase), USS (pkg.subpackage),
                                         USS (pkg.variant));
         case pkgid is
            when Pkgtypes.Package_Not_Installed => null;
            when others =>
               new_package := False;
               pkg.id := pkgid;
               QRY.get_package_files (db, pkgid, old_files);
               if onward then
                  onward := delete_satellite (db, pkgid, "pkg_users");
               end if;
               if onward then
                  onward := delete_satellite (db, pkgid, "pkg_groups");
               end if;
               if onward then
                  onward := delete_satellite (db, pkgid, "pkg_scripts");
               end if;
               if onward then
                  onward := delete_satellite (db, pkgid, "pkg_messages");
               end if;
               if onward then
                  onward := delete_satellite (db, pkgid, "pkg_licenses");
               end if;
               if onward then
                  onward := delete_satellite (db, pkgid, "pkg_libs_provided");
               end if;
               if onward then
                  onward := delete_satellite (db, pkgid, "pkg_libs_required");
               end if;
               if onward then
                  onward := delete_satellite (db, pkgid, "pkg_libs_adjacent");
               end if;
               if onward then
                  onward := delete_satellite (db, pkgid, "pkg_categories");
               end if;
               if onward then
                  onward := delete_satellite (db, pkgid, "pkg_directories");
               end if;
               if onward then
                  onward := delete_satellite (db, pkgid, "pkg_annotations");
               end if;
               if onward then
                  onward := delete_satellite (db, pkgid, "pkg_options");
               end if;
               if onward then
                  onward := delete_satellite (db, pkgid, "pkg_dependencies");
               end if;
               if onward then
                  onward := delete_satellite (db, pkgid, "pkg_files");
               end if;
               if onward then
                  onward := delete_satellite (db, pkgid, "pkg_triggers");
               end if;
               if onward then
                  onward := overwrite_main_pkg (db, pkgid, pkg);
               end if;
         end case;
      end if;

      if new_package then
         if onward then
            onward := run_prstmt_main_pkg (db, pkg);
         end if;
         if onward then
            pkg.id := QRY.package_installed (db,
                                             USS (pkg.namebase),
                                             USS (pkg.subpackage),
                                             USS (pkg.variant));
         end if;
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
         onward := run_prstmt_message (db, pkg);
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
         onward := run_prstmt_file (db, pkg);
      end if;
      if onward then
         onward := run_prstmt_trigger (db, pkg);
      end if;

      if onward then
         if not new_package then
            remove_orphaned_files (pkg, old_files);
         end if;

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
   procedure squawk_reset_error (stmt_type : prepared_statement) is
   begin
      Event.emit_error ("failed to reset " & stmt_type'Img & " prepared statement");
   end squawk_reset_error;


   ---------------------------
   --  run_prstmt_main_pkg  --
   ---------------------------
   function run_prstmt_main_pkg (db : in out RDB_Connection; pkg : Pkgtypes.A_Package) return Boolean
   is
      this_stmt : SQLite.thick_stmt renames db.prepared_statements (Database.main_pkg);
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
         squawk_reset_error (Database.main_pkg);
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
      SQLite.bind_string (this_stmt, 11, USS (pkg.rvndigest));
      SQLite.bind_integer (this_stmt, 12, SQLite.sql_int64 (pkg.rvnsize));
      SQLite.bind_integer (this_stmt, 13, SQLite.sql_int64 (pkg.flatsize));
      SQLite.bind_integer (this_stmt, 14, liclogic);
      SQLite.bind_integer (this_stmt, 15, automatic);
      SQLite.bind_integer (this_stmt, 16, SQLite.sql_int64 (pkg.install_time));

      debug_running_stmt (this_stmt);

      case SQLite.step (this_stmt) is
         when SQLite.no_more_data => return True;
         when SQLite.row_present | SQLite.something_else =>
            CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                         SQLite.get_expanded_sql (this_stmt));
            return False;
      end case;

   end run_prstmt_main_pkg;


   --------------------------
   --  overwrite_main_pkg  --
   --------------------------
   function overwrite_main_pkg
     (db    : RDB_Connection;
      pkgid : Pkgtypes.Package_ID;
      pkg   : Pkgtypes.A_Package) return Boolean
   is
      this_stmt : SQLite.thick_stmt;
      automatic : SQLite.sql_int64 := 0;
      liclogic  : SQLite.sql_int64;
      func      : constant String := "overwrite_main_pkg";
      sql       : constant String := "UPDATE packages SET namebase = ?1, subpackage = ?2, " &
        "variant = ?3, version = ?4, comment = ?5, desc = ?6, www = ?7, maintainer = ?8, " &
        "prefix = ?9, abi = ?10, flatsize = ?11, licenselogic = ?12, automatic = ?13, " &
        "installed = ?14 WHERE id = ?15";
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

      if not SQLite.prepare_sql (db.handle, sql, this_stmt) then
         CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func, sql);
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
      SQLite.bind_integer (this_stmt, 14, SQLite.sql_int64 (pkg.install_time));
      SQLite.bind_integer (this_stmt, 15, SQLite.sql_int64 (pkgid));

      debug_running_stmt (this_stmt);
      case SQLite.step (this_stmt) is
         when SQLite.no_more_data => return True;
         when SQLite.row_present | SQLite.something_else =>
            CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                         SQLite.get_expanded_sql (this_stmt));
            return False;
      end case;

   end overwrite_main_pkg;


   ---------------------------
   --  run_prstmt_category  --
   ---------------------------
   function run_prstmt_category (db : in out RDB_Connection; pkg : A_Package) return Boolean
   is
      main_stmt : SQLite.thick_stmt renames db.prepared_statements (Database.category);
      pack_stmt : SQLite.thick_stmt renames db.prepared_statements (Database.pkg_category);

      func       : constant String := "run_prstmt_category";
      keep_going : Boolean := True;

      procedure insert_main (Position : Pkgtypes.Text_List.Cursor)
      is
         category : constant String := USS (Pkgtypes.Text_List.Element (Position));
      begin
         if not keep_going then
            return;
         end if;
         if SQLite.reset_statement (main_stmt) then
            SQLite.bind_string (main_stmt, 1, category);
            debug_running_stmt (main_stmt);
            case SQLite.step (main_stmt) is
               when SQLite.no_more_data => null;
               when SQLite.row_present | SQLite.something_else =>
                  CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                               SQLite.get_expanded_sql (main_stmt));
                  keep_going := False;
            end case;
         else
            squawk_reset_error (Database.category);
            keep_going := False;
         end if;
      end insert_main;

      procedure insert_into_package (Position : Pkgtypes.Text_List.Cursor)
      is
         category : constant String := USS (Pkgtypes.Text_List.Element (Position));
      begin
         if not keep_going then
            return;
         end if;
         if SQLite.reset_statement (pack_stmt) then
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
         else
            squawk_reset_error (Database.pkg_category);
            keep_going := False;
         end if;
      end insert_into_package;
   begin
      pkg.categories.Iterate (insert_main'Access);
      pkg.categories.Iterate (insert_into_package'Access);

      return keep_going;
   end run_prstmt_category;


   --------------------------
   --  run_prstmt_license  --
   --------------------------
   function run_prstmt_license (db : in out RDB_Connection; pkg : A_Package) return Boolean
   is
      main_stmt : SQLite.thick_stmt renames db.prepared_statements (Database.license);
      pack_stmt : SQLite.thick_stmt renames db.prepared_statements (Database.pkg_license);

      func       : constant String := "run_prstmt_license";
      keep_going : Boolean := True;

      procedure insert_main (Position : Pkgtypes.Text_List.Cursor)
      is
         license : constant String := USS (Pkgtypes.Text_List.Element (Position));
      begin
         if not keep_going then
            return;
         end if;
         if SQLite.reset_statement (main_stmt) then
            SQLite.bind_string (main_stmt, 1, license);
            debug_running_stmt (main_stmt);
            case SQLite.step (main_stmt) is
               when SQLite.no_more_data => null;
               when SQLite.row_present | SQLite.something_else =>
                  CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                               SQLite.get_expanded_sql (main_stmt));
                  keep_going := False;
            end case;
         else
            squawk_reset_error (Database.license);
            keep_going := False;
         end if;
      end insert_main;

      procedure insert_into_package (Position : Pkgtypes.Text_List.Cursor)
      is
         license : constant String := USS (Pkgtypes.Text_List.Element (Position));
      begin
         if not keep_going then
            return;
         end if;
         if SQLite.reset_statement (pack_stmt) then
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
         else
            squawk_reset_error (Database.pkg_license);
            keep_going := False;
         end if;
      end insert_into_package;
   begin
      pkg.licenses.Iterate (insert_main'Access);
      pkg.licenses.Iterate (insert_into_package'Access);

      return keep_going;
   end run_prstmt_license;


   -----------------------
   --  run_prstmt_user  --
   -----------------------
   function run_prstmt_user (db : in out RDB_Connection; pkg : A_Package) return Boolean
   is
      main_stmt : SQLite.thick_stmt renames db.prepared_statements (Database.user);
      pack_stmt : SQLite.thick_stmt renames db.prepared_statements (Database.pkg_user);

      func       : constant String := "run_prstmt_user";
      keep_going : Boolean := True;

      procedure insert_main (Position : Pkgtypes.Text_List.Cursor)
      is
         user : constant String := USS (Pkgtypes.Text_List.Element (Position));
      begin
         if not keep_going then
            return;
         end if;
         if SQLite.reset_statement (main_stmt) then
            SQLite.bind_string (main_stmt, 1, user);
            debug_running_stmt (main_stmt);
            case SQLite.step (main_stmt) is
               when SQLite.no_more_data => null;
               when SQLite.row_present | SQLite.something_else =>
                  CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                               SQLite.get_expanded_sql (main_stmt));
                  keep_going := False;
            end case;
         else
            squawk_reset_error (Database.user);
            keep_going := False;
         end if;
      end insert_main;

      procedure insert_into_package (Position : Pkgtypes.Text_List.Cursor)
      is
         user : constant String := USS (Pkgtypes.Text_List.Element (Position));
      begin
         if not keep_going then
            return;
         end if;
         if SQLite.reset_statement (pack_stmt) then
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
         else
            squawk_reset_error (Database.pkg_user);
            keep_going := False;
         end if;
      end insert_into_package;
   begin
      pkg.users.Iterate (insert_main'Access);
      pkg.users.Iterate (insert_into_package'Access);

      return keep_going;
   end run_prstmt_user;


   ------------------------
   --  run_prstmt_group  --
   ------------------------
   function run_prstmt_group (db : in out RDB_Connection; pkg : A_Package) return Boolean
   is
      main_stmt : SQLite.thick_stmt renames db.prepared_statements (Database.group);
      pack_stmt : SQLite.thick_stmt renames db.prepared_statements (Database.pkg_group);

      func       : constant String := "run_prstmt_group";
      keep_going : Boolean := True;

      procedure insert_main (Position : Pkgtypes.Text_List.Cursor)
      is
         group : constant String := USS (Pkgtypes.Text_List.Element (Position));
      begin
         if not keep_going then
            return;
         end if;
         if SQLite.reset_statement (main_stmt) then
            SQLite.bind_string (main_stmt, 1, group);
            debug_running_stmt (main_stmt);
            case SQLite.step (main_stmt) is
               when SQLite.no_more_data => null;
               when SQLite.row_present | SQLite.something_else =>
                  CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                               SQLite.get_expanded_sql (main_stmt));
                  keep_going := False;
            end case;
         else
            squawk_reset_error (Database.group);
            keep_going := False;
         end if;
      end insert_main;

      procedure insert_into_package (Position : Pkgtypes.Text_List.Cursor)
      is
         group : constant String := USS (Pkgtypes.Text_List.Element (Position));
      begin
         if not keep_going then
            return;
         end if;
         if SQLite.reset_statement (pack_stmt) then
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
         else
            squawk_reset_error (Database.pkg_group);
            keep_going := False;
         end if;
      end insert_into_package;
   begin
      pkg.groups.Iterate (insert_main'Access);
      pkg.groups.Iterate (insert_into_package'Access);

      return keep_going;
   end run_prstmt_group;


   ----------------------------
   --  run_prstmt_directory  --
   ----------------------------
   function run_prstmt_directory (db : in out RDB_Connection; pkg : A_Package) return Boolean
   is
      main_stmt : SQLite.thick_stmt renames db.prepared_statements (Database.directory);
      pack_stmt : SQLite.thick_stmt renames db.prepared_statements (Database.pkg_directory);

      func       : constant String := "run_prstmt_directory";
      keep_going : Boolean := True;

      procedure insert_main (Position : Pkgtypes.Text_List.Cursor)
      is
         directory : constant String := USS (Pkgtypes.Text_List.Element (Position));
      begin
         if not keep_going then
            return;
         end if;
         if SQLite.reset_statement (main_stmt) then
            SQLite.bind_string (main_stmt, 1, directory);
            debug_running_stmt (main_stmt);
            case SQLite.step (main_stmt) is
               when SQLite.no_more_data => null;
               when SQLite.row_present | SQLite.something_else =>
                  CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                               SQLite.get_expanded_sql (main_stmt));
                  keep_going := False;
            end case;
         else
            squawk_reset_error (Database.directory);
            keep_going := False;
         end if;
      end insert_main;

      procedure insert_into_package (Position : Pkgtypes.Text_List.Cursor)
      is
         directory : constant String := USS (Pkgtypes.Text_List.Element (Position));
      begin
         if not keep_going then
            return;
         end if;
         if SQLite.reset_statement (pack_stmt) then
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
         else
            squawk_reset_error (Database.pkg_directory);
            keep_going := False;
         end if;
      end insert_into_package;
   begin
      pkg.directories.Iterate (insert_main'Access);
      pkg.directories.Iterate (insert_into_package'Access);

      return keep_going;
   end run_prstmt_directory;


   -------------------------
   --  run_prstmt_script  --
   -------------------------
   function run_prstmt_script (db : in out RDB_Connection; pkg : A_Package) return Boolean
   is
      main_stmt : SQLite.thick_stmt renames db.prepared_statements (Database.script);
      pack_stmt : SQLite.thick_stmt renames db.prepared_statements (Database.pkg_script);

      func       : constant String := "run_prstmt_script";
      keep_going : Boolean := True;
      phase_type : Natural := 0;
      phase_ndx  : Natural := 0;

      procedure insert_main (Position : Pkgtypes.Script_List.Cursor)
      is
         code : constant String := USS (Pkgtypes.Script_List.Element (Position).code);
      begin
         if not keep_going then
            return;
         end if;
         if SQLite.reset_statement (main_stmt) then
            SQLite.bind_string (main_stmt, 1, code);
            debug_running_stmt (main_stmt);
            case SQLite.step (main_stmt) is
               when SQLite.no_more_data => null;
               when SQLite.row_present | SQLite.something_else =>
                  CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                               SQLite.get_expanded_sql (main_stmt));
                  keep_going := False;
            end case;
         else
            squawk_reset_error (Database.script);
            keep_going := False;
         end if;
      end insert_main;

      procedure insert_into_package (Position : Pkgtypes.Script_List.Cursor)
      is
         --  package_id,script_type,type_index,arguments,(code)
         code : constant String := USS (Pkgtypes.Script_List.Element (Position).code);
         args : constant String := USS (Pkgtypes.Script_List.Element (Position).args);
      begin
         if not keep_going then
            return;
         end if;
         if SQLite.reset_statement (pack_stmt) then
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
         else
            squawk_reset_error (Database.pkg_script);
            keep_going := False;
         end if;
      end insert_into_package;
   begin
      for phase in Archive.Whitelist.package_phase'Range loop
         pkg.scripts (phase).Iterate (insert_main'Access);
      end loop;
      for phase in Archive.Whitelist.package_phase'Range loop
         phase_ndx := 0;                --  script array starts at 0

         pkg.scripts (phase).Iterate (insert_into_package'Access);
         phase_type := phase_type + 1;  --  type index starts at 0
      end loop;

      return keep_going;
   end run_prstmt_script;


   --------------------------
   --  run_prstmt_message  --
   --------------------------
   function run_prstmt_message (db : in out RDB_Connection; pkg : A_Package) return Boolean
   is
      pack_stmt : SQLite.thick_stmt renames db.prepared_statements (Database.pkg_message);

      func       : constant String := "run_prstmt_message";
      keep_going : Boolean := True;
      enum_mtype : Natural := 0;  --  install
      msg_ndx    : Natural := 0;  --  message index 0, 1, 2 ...

      procedure insert_into_package (Position : Pkgtypes.Message_List.Cursor)
      is
         myrec : Pkgtypes.Message_Parameters renames Pkgtypes.Message_List.Element (Position);
         message : constant String := USS (myrec.message);
         min_ver : constant String := USS (myrec.minimum_version);
         max_ver : constant String := USS (myrec.maximum_version);
      begin
         if not keep_going then
            return;
         end if;
         if SQLite.reset_statement (pack_stmt) then
            SQLite.bind_integer (pack_stmt, 1, SQLite.sql_int64 (pkg.id));
            SQLite.bind_integer (pack_stmt, 2, SQLite.sql_int64 (msg_ndx));
            SQLite.bind_integer (pack_stmt, 3, SQLite.sql_int64 (enum_mtype));
            SQLite.bind_string  (pack_stmt, 4, message);
            SQLite.bind_string  (pack_stmt, 5, min_ver);
            SQLite.bind_string  (pack_stmt, 6, max_ver);
            debug_running_stmt (pack_stmt);
            case SQLite.step (pack_stmt) is
               when SQLite.no_more_data => null;
               when SQLite.row_present | SQLite.something_else =>
                  CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                               SQLite.get_expanded_sql (pack_stmt));
                  keep_going := False;
            end case;
            msg_ndx := msg_ndx + 1;
         else
            squawk_reset_error (Database.pkg_message);
            keep_going := False;
         end if;
      end insert_into_package;
   begin
      for mtype in Pkgtypes.Message_Type'Range loop
         pkg.messages (mtype).iterate (insert_into_package'Access);
         enum_mtype := enum_mtype + 1;
      end loop;
      return keep_going;
   end run_prstmt_message;


   --------------------------
   --  run_prstmt_library  --
   --------------------------
   function run_prstmt_library (db : in out RDB_Connection; pkg : A_Package) return Boolean
   is
      main_stmt  : SQLite.thick_stmt renames db.prepared_statements (Database.library);
      pack_stmt1 : SQLite.thick_stmt renames db.prepared_statements (Database.pkg_adjacent_lib);
      pack_stmt2 : SQLite.thick_stmt renames db.prepared_statements (Database.pkg_provided_lib);
      pack_stmt3 : SQLite.thick_stmt renames db.prepared_statements (Database.pkg_required_lib);

      func       : constant String := "run_prstmt_library";
      keep_going : Boolean := True;

      procedure insert_main (Position : Pkgtypes.Text_List.Cursor)
      is
         library : constant String := USS (Pkgtypes.Text_List.Element (Position));
      begin
         if not keep_going then
            return;
         end if;
         if SQLite.reset_statement (main_stmt) then
            SQLite.bind_string (main_stmt, 1, library);
            debug_running_stmt (main_stmt);
            case SQLite.step (main_stmt) is
               when SQLite.no_more_data => null;
               when SQLite.row_present | SQLite.something_else =>
                  CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                               SQLite.get_expanded_sql (main_stmt));
                  keep_going := False;
            end case;
         else
            squawk_reset_error (Database.library);
            keep_going := False;
         end if;
      end insert_main;

      procedure insert_into_package1 (Position : Pkgtypes.Text_List.Cursor)
      is
         license : constant String := USS (Pkgtypes.Text_List.Element (Position));
      begin
         if not keep_going then
            return;
         end if;
         if SQLite.reset_statement (pack_stmt1) then
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
         else
            squawk_reset_error (Database.pkg_adjacent_lib);
            keep_going := False;
         end if;
      end insert_into_package1;

      procedure insert_into_package2 (Position : Pkgtypes.Text_List.Cursor)
      is
         license : constant String := USS (Pkgtypes.Text_List.Element (Position));
      begin
         if not keep_going then
            return;
         end if;
         if SQLite.reset_statement (pack_stmt2) then
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
         else
            squawk_reset_error (Database.pkg_provided_lib);
            keep_going := False;
         end if;
      end insert_into_package2;

      procedure insert_into_package3 (Position : Pkgtypes.Text_List.Cursor)
      is
         license : constant String := USS (Pkgtypes.Text_List.Element (Position));
      begin
         if not keep_going then
            return;
         end if;
         if SQLite.reset_statement (pack_stmt3) then
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
         else
            squawk_reset_error (Database.pkg_required_lib);
            keep_going := False;
         end if;
      end insert_into_package3;
   begin
      pkg.libs_adjacent.Iterate (insert_main'Access);
      pkg.libs_provided.Iterate (insert_main'Access);
      pkg.libs_required.Iterate (insert_main'Access);

      pkg.libs_adjacent.Iterate (insert_into_package1'Access);
      pkg.libs_provided.Iterate (insert_into_package2'Access);
      pkg.libs_required.Iterate (insert_into_package3'Access);

      return keep_going;
   end run_prstmt_library;


   -----------------------
   --  run_prstmt_note  --
   -----------------------
   function run_prstmt_note (db : in out RDB_Connection; pkg : A_Package) return Boolean
   is
      main_stmt : SQLite.thick_stmt renames db.prepared_statements (Database.note);
      pack_stmt : SQLite.thick_stmt renames db.prepared_statements (Database.pkg_note);

      func       : constant String := "run_prstmt_note";
      keep_going : Boolean := True;

      procedure insert_main (Position : Pkgtypes.NoteSet.Cursor)
      is
         note_key : constant String := USS (Pkgtypes.NoteSet.Key (Position));
      begin
         if not keep_going then
            return;
         end if;
         if SQLite.reset_statement (main_stmt) then
            SQLite.bind_string (main_stmt, 1, note_key);
            debug_running_stmt (main_stmt);
            case SQLite.step (main_stmt) is
               when SQLite.no_more_data => null;
               when SQLite.row_present | SQLite.something_else =>
                  CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                               SQLite.get_expanded_sql (main_stmt));
                  keep_going := False;
            end case;
         else
            squawk_reset_error (Database.note);
            keep_going := False;
         end if;
      end insert_main;

      procedure insert_into_package (Position : Pkgtypes.NoteSet.Cursor)
      is
         mynote : Pkgtypes.Note_Item renames Pkgtypes.NoteSet.Element (Position);
      begin
         if not keep_going then
            return;
         end if;
         if SQLite.reset_statement (pack_stmt) then
            SQLite.bind_integer (pack_stmt, 1, SQLite.sql_int64 (pkg.id));
            SQLite.bind_string  (pack_stmt, 2, USS (mynote.tag));
            SQLite.bind_string  (pack_stmt, 3, USS (mynote.note));
            if mynote.custom then
               SQLite.bind_integer (pack_stmt, 4, 1);
            else
               SQLite.bind_integer (pack_stmt, 4, 0);
            end if;
            debug_running_stmt (pack_stmt);
            case SQLite.step (pack_stmt) is
               when SQLite.no_more_data => null;
               when SQLite.row_present | SQLite.something_else =>
                  CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                               SQLite.get_expanded_sql (pack_stmt));
                  keep_going := False;
            end case;
         else
            squawk_reset_error (Database.pkg_note);
            keep_going := False;
         end if;
      end insert_into_package;
   begin
      pkg.annotations.Iterate (insert_main'Access);
      pkg.annotations.Iterate (insert_into_package'Access);

      return keep_going;
   end run_prstmt_note;


   --------------------------
   --  run_prstmt_trigger  --
   --------------------------
   function run_prstmt_trigger (db : in out RDB_Connection; pkg : A_Package) return Boolean
   is
      pack_stmt : SQLite.thick_stmt renames db.prepared_statements (Database.pkg_trigger);
      path_stmt : SQLite.thick_stmt renames db.prepared_statements (Database.trig_paths);

      func       : constant String := "run_prstmt_trigger";
      keep_going : Boolean := True;
      path_type  : SQLite.sql_int64 := 0;
      type_index : SQLite.sql_int64 := 0;

      procedure bump
      is
         use type SQLite.sql_int64;
      begin
         path_type := path_type + 1;
         type_index := 0;
      end bump;

      procedure insert_trigger_path (Position : Pkgtypes.Text_List.Cursor)
      is
         trigger_path : constant String := USS (Pkgtypes.Text_List.Element (Position));
      begin
         if not keep_going then
            return;
         end if;
         if SQLite.reset_statement (path_stmt) then
            SQLite.bind_integer (path_stmt, 1, SQLite.sql_int64 (pkg.id));
            SQLite.bind_integer (path_stmt, 2, path_type);
            SQLite.bind_integer (path_stmt, 3, type_index);
            SQLite.bind_string (path_stmt, 4, trigger_path);

            debug_running_stmt (path_stmt);
            case SQLite.step (path_stmt) is
               when SQLite.no_more_data => null;
               when SQLite.row_present | SQLite.something_else =>
                  CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                               SQLite.get_expanded_sql (path_stmt));
                  keep_going := False;
            end case;
         else
            squawk_reset_error (Database.trig_paths);
            keep_going := False;
         end if;
         type_index := SQLite."+" (type_index, 1);
      end insert_trigger_path;

      procedure inject_code (code : String; trig_type : SQLite.sql_int64) is
      begin
         if not keep_going then
            return;
         end if;
          if SQLite.reset_statement (pack_stmt) then
            SQLite.bind_integer (pack_stmt, 1, SQLite.sql_int64 (pkg.id));
            SQLite.bind_integer (pack_stmt, 2, trig_type);
            SQLite.bind_string (pack_stmt, 3, code);

            debug_running_stmt (pack_stmt);
            case SQLite.step (pack_stmt) is
               when SQLite.no_more_data => null;
               when SQLite.row_present | SQLite.something_else =>
                  CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                               SQLite.get_expanded_sql (pack_stmt));
                  keep_going := False;
            end case;
         else
            squawk_reset_error (Database.pkg_trigger);
            keep_going := False;
         end if;
      end inject_code;

      procedure insert_into_package (Position : Pkgtypes.Trigger_List.Cursor)
      is
         trig : Pkgtypes.A_Trigger renames Pkgtypes.Trigger_List.Element (Position);
      begin
         if not IsBlank (trig.cleanup_script) then
            inject_code (USS (trig.cleanup_script), 0);
         end if;

         if not IsBlank (trig.install_script) then
            --  Paths only used for triggers, not cleanup.
            --  Enforce validity checks at ravenadm

            path_type := 0;
            type_index := 0;

            inject_code (USS (trig.install_script), 1);

            trig.set_dir_path.Iterate (insert_trigger_path'Access);
            bump;
            trig.set_file_path.Iterate (insert_trigger_path'Access);
            bump;
            trig.set_file_glob.Iterate (insert_trigger_path'Access);
            bump;
            trig.set_file_regex.Iterate (insert_trigger_path'Access);
         end if;
      end insert_into_package;
   begin
      pkg.triggers.Iterate (insert_into_package'Access);
      return keep_going;
   end run_prstmt_trigger;


   -------------------------
   --  run_prstmt_option  --
   -------------------------
   function run_prstmt_option (db : in out RDB_Connection; pkg : A_Package) return Boolean
   is
      main_stmt : SQLite.thick_stmt renames db.prepared_statements (Database.option);
      pack_stmt : SQLite.thick_stmt renames db.prepared_statements (Database.pkg_option);

      func       : constant String := "run_prstmt_option";
      keep_going : Boolean := True;

      procedure insert_main (Position : Pkgtypes.NV_Pairs.Cursor)
      is
         opt_name : constant String := USS (Pkgtypes.NV_Pairs.Key (Position));
      begin
         if not keep_going then
            return;
         end if;
         if SQLite.reset_statement (main_stmt) then
            SQLite.bind_string (main_stmt, 1, opt_name);
            debug_running_stmt (main_stmt);
            case SQLite.step (main_stmt) is
               when SQLite.no_more_data => null;
               when SQLite.row_present | SQLite.something_else =>
                  CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                               SQLite.get_expanded_sql (main_stmt));
                  keep_going := False;
            end case;
         else
            squawk_reset_error (Database.option);
            keep_going := False;
         end if;
      end insert_main;

      procedure insert_into_package (Position : Pkgtypes.NV_Pairs.Cursor)
      is
         opt_name  : constant String := USS (Pkgtypes.NV_Pairs.Key (Position));
         opt_value : constant String := USS (Pkgtypes.NV_Pairs.Element (Position));
      begin
         if not keep_going then
            return;
         end if;
         if SQLite.reset_statement (pack_stmt) then
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
         else
            squawk_reset_error (Database.pkg_option);
            keep_going := False;
         end if;
      end insert_into_package;
   begin
      pkg.options.Iterate (insert_main'Access);
      pkg.options.Iterate (insert_into_package'Access);

      return keep_going;
   end run_prstmt_option;


   -------------------------
   --  run_prstmt_depend  --
   -------------------------
   function run_prstmt_depend (db : in out RDB_Connection; pkg : A_Package) return Boolean
   is
      main_stmt : SQLite.thick_stmt renames db.prepared_statements (Database.dependency);
      pack_stmt : SQLite.thick_stmt renames db.prepared_statements (Database.pkg_dependency);

      func       : constant String := "run_prstmt_depend";
      keep_going : Boolean := True;

      procedure insert_main (Position : Pkgtypes.NV_Pairs.Cursor)
      is
         nsv     : constant String := USS (Pkgtypes.NV_Pairs.Key (Position));
         version : constant String := USS (Pkgtypes.NV_Pairs.Element (Position));
      begin
         if not keep_going then
            return;
         end if;
         if SQLite.reset_statement (main_stmt) then
            SQLite.bind_string (main_stmt, 1, nsv);
            SQLite.bind_string (main_stmt, 2, version);
            debug_running_stmt (main_stmt);
            case SQLite.step (main_stmt) is
               when SQLite.no_more_data => null;
               when SQLite.row_present | SQLite.something_else =>
                  CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                         SQLite.get_expanded_sql (main_stmt));
                  keep_going := False;
            end case;
         else
            squawk_reset_error (Database.dependency);
            keep_going := False;
         end if;
      end insert_main;

      procedure insert_into_package (Position : Pkgtypes.NV_Pairs.Cursor)
      is
         nsv     : constant String := USS (Pkgtypes.NV_Pairs.Key (Position));
         version : constant String := USS (Pkgtypes.NV_Pairs.Element (Position));
      begin
         if not keep_going then
            return;
         end if;
         if SQLite.reset_statement (pack_stmt) then
            SQLite.bind_integer (pack_stmt, 1, SQLite.sql_int64 (pkg.id));
            SQLite.bind_string  (pack_stmt, 2, nsv);
            SQLite.bind_string  (pack_stmt, 3, version);
            debug_running_stmt (pack_stmt);
            case SQLite.step (pack_stmt) is
               when SQLite.no_more_data => null;
               when SQLite.row_present | SQLite.something_else =>
                  CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                               SQLite.get_expanded_sql (pack_stmt));
                  keep_going := False;
            end case;
         else
            squawk_reset_error (Database.pkg_dependency);
            keep_going := False;
         end if;
      end insert_into_package;
   begin
      pkg.dependencies.Iterate (insert_main'Access);
      pkg.dependencies.Iterate (insert_into_package'Access);

      return keep_going;
   end run_prstmt_depend;


   ------------------------
   --  delete_satellite  --
   ------------------------
   function delete_satellite
     (db    : RDB_Connection;
      pkgid : Pkgtypes.Package_ID;
      table : String) return Boolean
   is
      sql : constant String := "DELETE FROM " & table & " WHERE package_id =" & pkgid'Img & ";";
   begin
      case CommonSQL.exec (db.handle, sql) is
         when RESULT_OK => return True;
         when others => return False;
      end case;
   end delete_satellite;


   -----------------------------
   --  remove_orphaned_files  --
   -----------------------------
   procedure remove_orphaned_files
     (pkg   : Pkgtypes.A_Package;
      old   : Archive.Unpack.file_records.Vector)
   is
      new_files : Pkgtypes.Text_List.Vector;

      procedure push_to_stack (Position : Pkgtypes.File_List.Cursor)
      is
         mypath : Text renames Pkgtypes.File_List.Element (Position).path;
      begin
         new_files.Append (mypath);
      end push_to_stack;

      procedure process_old_file (Position : Archive.Unpack.file_records.Cursor)
      is
         old_file : constant String := USS (Archive.Unpack.file_records.Element (Position).path);
      begin
         if not new_files.Contains (Archive.Unpack.file_records.Element (Position).path) then
            begin
               Event.emit_notice ("Deleting old file not present in the replacement package: "
                                    & old_file);
               Ada.Directories.Delete_File (old_file);
            exception
               when others =>
                  Event.emit_error ("Failed to remove orphaned file: " & old_file);
            end;
         end if;
      end process_old_file;
   begin
      pkg.files.Iterate (push_to_stack'Access);
      old.Iterate (process_old_file'Access);
   end remove_orphaned_files;


   -----------------------
   --  run_prstmt_file  --
   -----------------------
   function run_prstmt_file (db : in out RDB_Connection; pkg : A_Package) return Boolean
   is
      permissive : Boolean := RCU.config_setting (RCU.CFG.permissive);
      pack_stmt  : SQLite.thick_stmt renames db.prepared_statements (Database.pkg_file);

      func       : constant String := "run_prstmt_file";
      keep_going : Boolean := True;

      function permissive_info return String is
      begin
         if permissive then
            return " ignored by permissive mode";
         end if;
         return "";
      end permissive_info;

      procedure insert_into_package (Position : Pkgtypes.File_List.Cursor)
      is
         path  : constant String := USS (Pkgtypes.File_List.Element (Position).path);
         b3sum : constant String := String (Pkgtypes.File_List.Element (Position).digest);
      begin
         if not keep_going then
            return;
         end if;
         if SQLite.reset_statement (pack_stmt) then
            SQLite.bind_integer (pack_stmt, 1, SQLite.sql_int64 (pkg.id));
            SQLite.bind_string  (pack_stmt, 2, path);
            SQLite.bind_string  (pack_stmt, 3, b3sum);
            debug_running_stmt (pack_stmt);
            case SQLite.step (pack_stmt) is
               when SQLite.no_more_data => null;
               when SQLite.row_present =>
                  --  This should be impossible
                  CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                               SQLite.get_expanded_sql (pack_stmt));
                  keep_going := False;
               when SQLite.something_else =>
                  --  Probably a constrainst error due to path already inserted.
                  --  If permissive, keep going, otherwise throw the error
                  declare
                     these_packages : Pkgtypes.Package_Set.Vector;
                  begin
                     QRY.rvn_which (db, path, False, these_packages);
                     if these_packages.Is_Empty then
                        --  Stray entry in the files table not related to any known package:
                        --  overwrite this
                        if not overwrite_file (db, pkg.id, path, b3sum) then
                           Event.emit_error ("Failed to overwrite orphaned file: " & path);
                           keep_going := False;
                        end if;
                     else
                        declare
                           NSVV : constant String := USS (these_packages.Element (0).namebase) &
                             LAT.Tilde & USS (these_packages.Element (0).subpackage) &
                             LAT.Tilde & USS (these_packages.Element (0).variant) &
                             LAT.Tilde & USS (these_packages.Element (0).version);
                           newpkg : constant String := USS (pkg.namebase) &
                             LAT.Tilde & USS (pkg.subpackage) &
                             LAT.Tilde & USS (pkg.variant) &
                             LAT.Tilde & USS (pkg.version);
                        begin
                           Event.emit_error
                             (newpkg & " conflicts with " & NSVV &
                                " (installs files into the same place). Problematic file: " &
                                path & permissive_info);
                           if not permissive then
                               keep_going := False;
                           end if;
                        end;
                     end if;
                  end;
            end case;
         else
            squawk_reset_error (Database.pkg_dependency);
            keep_going := False;
         end if;
      end insert_into_package;
   begin
      if Context.reveal_developer_mode then
         permissive := False;
      end if;
      pkg.files.Iterate (insert_into_package'Access);

      return keep_going;
   end run_prstmt_file;


   ----------------------
   --  overwrite_file  --
   ----------------------
   function overwrite_file
     (db    : RDB_Connection;
      pkgid : Pkgtypes.Package_ID;
      path  : String;
      b3sum : String) return Boolean
   is
      func : constant String := "overwrite_file";
      sql  : constant String := "INSERT OR REPLACE INTO pkg_files(package_id,path,b3digest) " &
                                "VALUES(?1,?2,?3)";
      new_stmt : SQLite.thick_stmt;
      success  : Boolean := False;
   begin
      if not SQLite.prepare_sql (db.handle, sql, new_stmt) then
         CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func, sql);
         return False;
      end if;
      SQLite.bind_integer (new_stmt, 1, SQLite.sql_int64 (pkgid));
      SQLite.bind_string  (new_stmt, 2, path);
      SQLite.bind_string  (new_stmt, 3, b3sum);
      debug_running_stmt (new_stmt);
      case SQLite.step (new_stmt) is
         when SQLite.no_more_data =>
            success := True;
         when SQLite.row_present | SQLite.something_else =>
            CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                         SQLite.get_expanded_sql (new_stmt));
      end case;
      SQLite.finalize_statement (new_stmt);
      return success;
   end overwrite_file;


   ---------------------------
   --  rdb_reset_automatic  --
   ---------------------------
   function rdb_reset_automatic
     (db  : in out RDB_Connection;
      pkg : in out Pkgtypes.A_Package) return Boolean
   is
      new_setting : SQLite.sql_int64 := 0;
      success  : Boolean := False;
      new_stmt : SQLite.thick_stmt;
      func : constant String := "rdb_reset_automatic";
      sql : constant String :=
        "UPDATE packages SET automatic = ? " &
        "WHERE namebase = ? AND subpackage = ? AND variant = ?";
   begin
      if not SQLite.prepare_sql (db.handle, sql, new_stmt) then
         CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func, sql);
         return False;
      end if;

      if pkg.automatic then
         new_setting := 1;
      end if;
      SQLite.bind_integer (new_stmt, 1, new_setting);
      SQLite.bind_string  (new_stmt, 2, USS (pkg.namebase));
      SQLite.bind_string  (new_stmt, 3, USS (pkg.subpackage));
      SQLite.bind_string  (new_stmt, 4, USS (pkg.variant));
       debug_running_stmt (new_stmt);
      case SQLite.step (new_stmt) is
         when SQLite.no_more_data =>
            success := True;
         when SQLite.row_present | SQLite.something_else =>
            CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                         SQLite.get_expanded_sql (new_stmt));
      end case;
      SQLite.finalize_statement (new_stmt);
      return success;
   end rdb_reset_automatic;


end Raven.Database.Pkgs;
