--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Event;
with Raven.Strings;
with Raven.Database.CommonSQL;
with Raven.Database.Operations;

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
      pkg.categories.Iterate (insert_main'Access);

      if keep_going then
         if not SQLite.reset_statement (pack_stmt) then
            squawk_reset_error (SCH.pkg_license);
            return False;
         end if;
         pkg.categories.Iterate (insert_into_package'Access);
      end if;

      return keep_going;
   end run_prstmt_license;



end Raven.Database.Pkgs;
