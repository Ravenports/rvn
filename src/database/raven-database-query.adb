--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with SQLite;
with Blake_3;
with Raven.Event;
with Raven.Strings;
with Raven.Database.CommonSQL;

use Raven.Strings;

package body Raven.Database.Query is

   -------------------------
   --  package_installed  --
   -------------------------
   function package_installed
     (db         : RDB_Connection;
      namebase   : String;
      subpackage : String;
      variant    : String) return Pkgtypes.Package_ID
   is
      func : constant String := "package_installed";
      sql : constant String := "SELECT id FROM packages where namebase = ?1 " &
                               "AND subpackage = ?2 AND variant = ?3";
      new_stmt : SQLite.thick_stmt;
      installed : Pkgtypes.Package_ID := Pkgtypes.Package_Not_Installed;
   begin
      if not SQLite.prepare_sql (db.handle, sql, new_stmt) then
         CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func, sql);
         return installed;
      end if;
      SQLite.bind_string (new_stmt, 1, namebase);
      SQLite.bind_string (new_stmt, 2, subpackage);
      SQLite.bind_string (new_stmt, 3, variant);
      debug_running_stmt (new_stmt);

      case SQLite.step (new_stmt) is
         when SQLite.no_more_data => null;
         when SQLite.row_present =>
            installed := Pkgtypes.Package_ID (SQLite.retrieve_integer (new_stmt, 0));
         when SQLite.something_else =>
            CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                         SQLite.get_expanded_sql (new_stmt));
      end case;
      SQLite.finalize_statement (new_stmt);
      return installed;
   end package_installed;


   -------------------------
   --  get_package_files  --
   -------------------------
   procedure get_package_files
     (db         : RDB_Connection;
      pkg_id     : Pkgtypes.Package_ID;
      files      : in out Archive.Unpack.file_records.Vector)
   is
      func : constant String := "get_package_files";
      sql  : constant String := "SELECT path, b3digest FROM pkg_files where package_id = ?1";
      new_stmt : SQLite.thick_stmt;
   begin
      files.Clear;
      if not SQLite.prepare_sql (db.handle, sql, new_stmt) then
         CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func, sql);
         return;
      end if;
      SQLite.bind_integer (new_stmt, 1, SQLite.sql_int64 (pkg_id));
      debug_running_stmt (new_stmt);

      loop
         case SQLite.step (new_stmt) is
            when SQLite.row_present =>
               declare
                  path  : constant String := SQLite.retrieve_string (new_stmt, 0);
                  b3sum : constant String := SQLite.retrieve_string (new_stmt, 1);
                  myrec : Archive.Unpack.file_record;
               begin
                  myrec.path   := SUS (path);
                  myrec.digest := Blake_3.blake3_hash_hex (b3sum);
                  files.Append (myrec);
               exception
                  when others =>
                     myrec.digest := (others => '0');
                     files.Append (myrec);
               end;
            when SQLite.something_else =>
               CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                            SQLite.get_expanded_sql (new_stmt));
            when SQLite.no_more_data => exit;
         end case;
      end loop;
      SQLite.finalize_statement (new_stmt);
   end get_package_files;


   -----------------
   --  rvn_which  --
   -----------------
   procedure rvn_which
     (db         : RDB_Connection;
      query_path : String;
      use_glob   : Boolean;
      packages   : in out Pkgtypes.Package_Set.Vector)
   is
      function comparison return String is
      begin
         if use_glob then
            return "GLOB ?1 ";
         end if;
         return "= ?1 ";
      end comparison;

      func : constant String := "rvn_which";
      sql  : constant String :=
        "SELECT p.id, p.namebase, p.subpackage, p.variant, p.version, f.path " &
        "FROM packages as p " &
        "LEFT JOIN pkg_files as f ON p.id = f.package_id " &
        "WHERE f.path " & comparison &
        "ORDER BY p.id, f.path;";

      new_stmt : SQLite.thick_stmt;
      temporary_packages : Pkgtypes.Package_Set.Vector;
      temporary_ids      : Pkgtypes.ID_Set.Vector;

      procedure assemble (Position : Pkgtypes.ID_Set.Cursor)
      is
         this_pkgid : Pkgtypes.Package_ID := Pkgtypes.ID_Set.Element (Position);
         new_rec : Pkgtypes.A_Package;
         file_tray : Pkgtypes.File_Item;

         procedure scan (Position : Pkgtypes.Package_Set.Cursor)
         is
            use type Pkgtypes.Package_ID;
            mypkg : Pkgtypes.A_Package renames Pkgtypes.Package_Set.Element (Position);
         begin
            if mypkg.id = this_pkgid then
               file_tray.path := mypkg.comment;

               new_rec.id := mypkg.id;
               new_rec.namebase := mypkg.namebase;
               new_rec.subpackage := mypkg.subpackage;
               new_rec.variant := mypkg.variant;
               new_rec.version := mypkg.version;
               new_rec.files.Append (file_tray);
            end if;
         end scan;
      begin
         temporary_packages.Iterate (scan'Access);
         packages.Append (new_rec);
      end assemble;
   begin
      packages.clear;
      if not SQLite.prepare_sql (db.handle, sql, new_stmt) then
         CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func, sql);
         return;
      end if;
      SQLite.bind_string (new_stmt, 1, query_path);
      debug_running_stmt (new_stmt);

      loop
         case SQLite.step (new_stmt) is
            when SQLite.row_present =>
               declare
                  pkgid : constant Pkgtypes.Package_ID :=
                                   Pkgtypes.Package_ID (SQLite.retrieve_integer (new_stmt, 0));
                  namebase   : constant String := SQLite.retrieve_string (new_stmt, 1);
                  subpackage : constant String := SQLite.retrieve_string (new_stmt, 2);
                  variant    : constant String := SQLite.retrieve_string (new_stmt, 3);
                  version    : constant String := SQLite.retrieve_string (new_stmt, 4);
                  file_path  : constant String := SQLite.retrieve_string (new_stmt, 5);
                  myrec : Pkgtypes.A_Package;
               begin
                  myrec.id         := pkgid;
                  myrec.namebase   := SUS (namebase);
                  myrec.subpackage := SUS (subpackage);
                  myrec.variant    := SUS (variant);
                  myrec.version    := SUS (version);
                  myrec.comment    := SUS (file_path);
                  temporary_packages.Append (myrec);
                  if not temporary_ids.Contains (pkgid) then
                     temporary_ids.Append (pkgid);
                  end if;
               end;
            when SQLite.something_else =>
               CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                            SQLite.get_expanded_sql (new_stmt));
            when SQLite.no_more_data => exit;
         end case;
      end loop;
      SQLite.finalize_statement (new_stmt);
      temporary_ids.Iterate (assemble'Access);
   end rvn_which;


   ------------------------
   --  provides_library  --
   ------------------------
   procedure provides_library
     (db         : RDB_Connection;
      lib_soname : String;
      packages   : in out Pkgtypes.Package_Set.Vector)
   is
      func : constant String := "provides_library";
      sql  : constant String :=
        "SELECT p.id, p.namebase, p.subpackage, p.variant, p.version " &
        "FROM packages as p, pkg_libs_provided as prv, libraries as l " &
        "WHERE p.id = prv.package_id AND prv.library_id = l.library_id AND l.name = ?1";
      new_stmt : SQLite.thick_stmt;
   begin
      packages.clear;
      if not SQLite.prepare_sql (db.handle, sql, new_stmt) then
         CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func, sql);
         return;
      end if;
      SQLite.bind_string (new_stmt, 1, lib_soname);
      debug_running_stmt (new_stmt);

      loop
         case SQLite.step (new_stmt) is
            when SQLite.row_present =>
               declare
                  pkgid : constant Pkgtypes.Package_ID :=
                                   Pkgtypes.Package_ID (SQLite.retrieve_integer (new_stmt, 0));
                  namebase   : constant String := SQLite.retrieve_string (new_stmt, 1);
                  subpackage : constant String := SQLite.retrieve_string (new_stmt, 2);
                  variant    : constant String := SQLite.retrieve_string (new_stmt, 3);
                  version    : constant String := SQLite.retrieve_string (new_stmt, 4);
                  myrec : Pkgtypes.A_Package;
               begin
                  myrec.id         := pkgid;
                  myrec.namebase   := SUS (namebase);
                  myrec.subpackage := SUS (subpackage);
                  myrec.variant    := SUS (variant);
                  myrec.version    := SUS (version);
                  packages.Append (myrec);
               end;
            when SQLite.something_else =>
               CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                            SQLite.get_expanded_sql (new_stmt));
            when SQLite.no_more_data =>
               exit;
         end case;
      end loop;
      SQLite.finalize_statement (new_stmt);

   end provides_library;


   ------------------------
   --  requires_library  --
   ------------------------
   procedure requires_library
     (db         : RDB_Connection;
      lib_soname : String;
      packages   : in out Pkgtypes.Package_Set.Vector)
   is
      func : constant String := "requires_library";
      sql  : constant String :=
        "SELECT p.id, p.namebase, p.subpackage, p.variant, p.version " &
        "FROM packages as p, pkg_libs_required as req, libraries as l " &
        "WHERE p.id = req.package_id AND req.library_id = l.library_id AND l.name = ?1";
      new_stmt : SQLite.thick_stmt;
   begin
      packages.clear;
      if not SQLite.prepare_sql (db.handle, sql, new_stmt) then
         CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func, sql);
         return;
      end if;
      SQLite.bind_string (new_stmt, 1, lib_soname);
      debug_running_stmt (new_stmt);

      loop
         case SQLite.step (new_stmt) is
            when SQLite.row_present =>
               declare
                  pkgid : constant Pkgtypes.Package_ID :=
                                   Pkgtypes.Package_ID (SQLite.retrieve_integer (new_stmt, 0));
                  namebase   : constant String := SQLite.retrieve_string (new_stmt, 1);
                  subpackage : constant String := SQLite.retrieve_string (new_stmt, 2);
                  variant    : constant String := SQLite.retrieve_string (new_stmt, 3);
                  version    : constant String := SQLite.retrieve_string (new_stmt, 4);
                  myrec : Pkgtypes.A_Package;
               begin
                  myrec.id         := pkgid;
                  myrec.namebase   := SUS (namebase);
                  myrec.subpackage := SUS (subpackage);
                  myrec.variant    := SUS (variant);
                  myrec.version    := SUS (version);
                  packages.Append (myrec);
               end;
            when SQLite.something_else =>
               CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                            SQLite.get_expanded_sql (new_stmt));
            when SQLite.no_more_data =>
               exit;
         end case;
      end loop;
      SQLite.finalize_statement (new_stmt);
   end requires_library;


   ---------------------------
   --  all_remote_packages  --
   ---------------------------
   procedure all_remote_packages
     (db         : RDB_Connection;
      file_map   : in out Pkgtypes.NV_Pairs.Map)
   is
      func : constant String := "all_remote_packages";
      sql  : constant String :=
        "SELECT namebase, subpackage, variant, version, rvndigest FROM packages";
      new_stmt : SQLite.thick_stmt;
   begin
      file_map.Clear;
      if not SQLite.prepare_sql (db.handle, sql, new_stmt) then
         CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func, sql);
         return;
      end if;
      debug_running_stmt (new_stmt);

      loop
         case SQLite.step (new_stmt) is
            when SQLite.row_present =>
               declare
                  namebase   : constant String := SQLite.retrieve_string (new_stmt, 0);
                  subpackage : constant String := SQLite.retrieve_string (new_stmt, 1);
                  variant    : constant String := SQLite.retrieve_string (new_stmt, 2);
                  version    : constant String := SQLite.retrieve_string (new_stmt, 3);
                  digest32   : constant String := SQLite.retrieve_string (new_stmt, 4);
               begin
                  if digest32'Length = 64 then
                     declare
                        digest10 : constant String (1 .. 10) :=
                          digest32 (digest32'First .. digest32'first + 9);
                        filename : constant String := namebase & "-" & subpackage & "-" &
                          variant & "-" & version & "~" & digest10 & extension;
                        dkey : constant Text := SUS (digest10);
                     begin
                        if file_map.Contains (dkey) then
                           Event.emit_debug (high_level,
                                             func & ": hit collison, skipped " & filename);
                        else
                           file_map.Insert (dkey, SUS (filename));
                        end if;
                     end;
                  else
                     Event.emit_debug (moderate, "digest of " & namebase & "-" & subpackage &
                                         "-" & variant & " is not 64 chars long, ignored");
                  end if;
               end;
            when SQLite.something_else =>
               CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                            SQLite.get_expanded_sql (new_stmt));
            when SQLite.no_more_data =>
               exit;
         end case;
      end loop;
      SQLite.finalize_statement (new_stmt);

   end all_remote_packages;


   ---------------------------
   --  package_measurement  --
   ---------------------------
   procedure package_measurement
     (db           : RDB_Connection;
      measurements : in out A_Measurement_Set)
   is
      func : constant String := "package_measurement";
      sql  : constant String :=
        "SELECT COUNT(*) as num_packages, " &
        "COUNT(DISTINCT(namebase)) as num_ports, " &
        "COUNT(DISTINCT CONCAT(namebase, variant)) as num_variants, " &
        "SUM(flatsize) as sum_flatsize, " &
        "SUM(rvnsize) as sum_pkgsize " &
        "FROM packages";
      new_stmt : SQLite.thick_stmt;
   begin
      if not SQLite.prepare_sql (db.handle, sql, new_stmt) then
         CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func, sql);
         return;
      end if;
      debug_running_stmt (new_stmt);

      case SQLite.step (new_stmt) is
         when SQLite.no_more_data => null;
         when SQLite.row_present =>
            measurements.num_packages := Natural (SQLite.retrieve_integer (new_stmt, 0));
            measurements.num_ports    := Natural (SQLite.retrieve_integer (new_stmt, 1));
            measurements.num_variants := Natural (SQLite.retrieve_integer (new_stmt, 2));
            measurements.sum_flatsize := Pkgtypes.Package_Size (SQLite.retrieve_integer
                                                                (new_stmt, 3));
            measurements.sum_pkgsize  := Pkgtypes.Package_Size (SQLite.retrieve_integer
                                                                (new_stmt, 4));
         when SQLite.something_else =>
            CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                         SQLite.get_expanded_sql (new_stmt));
      end case;
      SQLite.finalize_statement (new_stmt);

   end package_measurement;


   ----------------------------
   --  finish_package_users  --
   ----------------------------
   procedure finish_package_users (db         : RDB_Connection;
                                   incomplete : in out Pkgtypes.A_Package)
   is
      new_stmt : SQLite.thick_stmt;
      func : constant String := "finish_package_users";
      sql : constant String :=
        "SELECT x.user_id, ml.name FROM pkg_users x " &
        "JOIN users ml ON x.user_id = ml.user_id WHERE x.package_id = ? ORDER BY ml.name";
   begin
      incomplete.users.clear;
      if not SQLite.prepare_sql (db.handle, sql, new_stmt) then
         CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func, sql);
         return;
      end if;
      SQLite.bind_integer (new_stmt, 1, SQLite.sql_int64 (incomplete.id));
      debug_running_stmt (new_stmt);

      loop
         case SQLite.step (new_stmt) is
            when SQLite.no_more_data => exit;
            when SQLite.row_present =>
               incomplete.users.Append (SUS (SQLite.retrieve_string (new_stmt, 1)));
            when SQLite.something_else =>
               CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                            SQLite.get_expanded_sql (new_stmt));
         end case;
      end loop;
      SQLite.finalize_statement (new_stmt);

   end finish_package_users;


   -----------------------------
   --  finish_package_groups  --
   -----------------------------
   procedure finish_package_groups (db         : RDB_Connection;
                                    incomplete : in out Pkgtypes.A_Package)
   is
      new_stmt : SQLite.thick_stmt;
      func : constant String := "finish_package_groups";
      sql : constant String :=
        "SELECT x.group_id, ml.name FROM pkg_groups x " &
        "JOIN groups ml ON x.group_id = ml.group_id WHERE x.package_id = ? ORDER BY ml.name";
   begin
      incomplete.groups.clear;
      if not SQLite.prepare_sql (db.handle, sql, new_stmt) then
         CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func, sql);
         return;
      end if;
      SQLite.bind_integer (new_stmt, 1, SQLite.sql_int64 (incomplete.id));
      debug_running_stmt (new_stmt);

      loop
         case SQLite.step (new_stmt) is
            when SQLite.no_more_data => exit;
            when SQLite.row_present =>
               incomplete.groups.Append (SUS (SQLite.retrieve_string (new_stmt, 1)));
            when SQLite.something_else =>
               CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                            SQLite.get_expanded_sql (new_stmt));
         end case;
      end loop;
      SQLite.finalize_statement (new_stmt);

   end finish_package_groups;


   -------------------------------
   --  finish_package_licenses  --
   -------------------------------
   procedure finish_package_licenses (db         : RDB_Connection;
                                      incomplete : in out Pkgtypes.A_Package)
   is
      new_stmt : SQLite.thick_stmt;
      func : constant String := "finish_package_licenses";
      sql : constant String :=
        "SELECT x.license_id, ml.name FROM pkg_licenses x " &
        "JOIN licenses ml ON x.license_id = ml.license_id WHERE x.package_id = ? ORDER BY ml.name";
   begin
      incomplete.licenses.clear;
      if not SQLite.prepare_sql (db.handle, sql, new_stmt) then
         CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func, sql);
         return;
      end if;
      SQLite.bind_integer (new_stmt, 1, SQLite.sql_int64 (incomplete.id));
      debug_running_stmt (new_stmt);

      loop
         case SQLite.step (new_stmt) is
            when SQLite.no_more_data => exit;
            when SQLite.row_present =>
               incomplete.licenses.Append (SUS (SQLite.retrieve_string (new_stmt, 1)));
            when SQLite.something_else =>
               CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                            SQLite.get_expanded_sql (new_stmt));
         end case;
      end loop;
      SQLite.finalize_statement (new_stmt);

   end finish_package_licenses;


    --------------------------------
   --  finish_package_categories  --
   ---------------------------------
   procedure finish_package_categories (db         : RDB_Connection;
                                        incomplete : in out Pkgtypes.A_Package)
   is
      new_stmt : SQLite.thick_stmt;
      func : constant String := "finish_package_categories";
      sql : constant String :=
        "SELECT x.category_id, ml.name FROM pkg_categories x " &
        "JOIN categories ml ON x.category_id = ml.category_id " &
        "WHERE x.package_id = ? ORDER BY ml.name";
   begin
      incomplete.categories.clear;
      if not SQLite.prepare_sql (db.handle, sql, new_stmt) then
         CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func, sql);
         return;
      end if;
      SQLite.bind_integer (new_stmt, 1, SQLite.sql_int64 (incomplete.id));
      debug_running_stmt (new_stmt);

      loop
         case SQLite.step (new_stmt) is
            when SQLite.no_more_data => exit;
            when SQLite.row_present =>
               incomplete.categories.Append (SUS (SQLite.retrieve_string (new_stmt, 1)));
            when SQLite.something_else =>
               CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                            SQLite.get_expanded_sql (new_stmt));
         end case;
      end loop;
      SQLite.finalize_statement (new_stmt);

   end finish_package_categories;


   ------------------------------------
   --  finish_package_libs_required  --
   ------------------------------------
   procedure finish_package_libs_required (db         : RDB_Connection;
                                           incomplete : in out Pkgtypes.A_Package)
   is
      new_stmt : SQLite.thick_stmt;
      func : constant String := "finish_package_libs_required";
      sql : constant String :=
        "SELECT x.library_id, ml.name FROM pkg_libs_required x " &
        "JOIN libraries ml ON x.library_id = ml.library_id WHERE x.package_id = ? ORDER BY ml.name";
   begin
      incomplete.libs_required.clear;
      if not SQLite.prepare_sql (db.handle, sql, new_stmt) then
         CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func, sql);
         return;
      end if;
      SQLite.bind_integer (new_stmt, 1, SQLite.sql_int64 (incomplete.id));
      debug_running_stmt (new_stmt);

      loop
         case SQLite.step (new_stmt) is
            when SQLite.no_more_data => exit;
            when SQLite.row_present =>
               incomplete.libs_required.Append (SUS (SQLite.retrieve_string (new_stmt, 1)));
            when SQLite.something_else =>
               CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                            SQLite.get_expanded_sql (new_stmt));
         end case;
      end loop;
      SQLite.finalize_statement (new_stmt);

   end finish_package_libs_required;


   ------------------------------------
   --  finish_package_libs_provided  --
   ------------------------------------
   procedure finish_package_libs_provided (db         : RDB_Connection;
                                           incomplete : in out Pkgtypes.A_Package)
   is
      new_stmt : SQLite.thick_stmt;
      func : constant String := "finish_package_libs_provided";
      sql : constant String :=
        "SELECT x.library_id, ml.name FROM pkg_libs_provided x " &
        "JOIN libraries ml ON x.library_id = ml.library_id WHERE x.package_id = ? ORDER BY ml.name";
   begin
      incomplete.libs_provided.clear;
      if not SQLite.prepare_sql (db.handle, sql, new_stmt) then
         CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func, sql);
         return;
      end if;
      SQLite.bind_integer (new_stmt, 1, SQLite.sql_int64 (incomplete.id));
      debug_running_stmt (new_stmt);

      loop
         case SQLite.step (new_stmt) is
            when SQLite.no_more_data => exit;
            when SQLite.row_present =>
               incomplete.libs_provided.Append (SUS (SQLite.retrieve_string (new_stmt, 1)));
            when SQLite.something_else =>
               CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                            SQLite.get_expanded_sql (new_stmt));
         end case;
      end loop;
      SQLite.finalize_statement (new_stmt);

   end finish_package_libs_provided;


   ------------------------------------
   --  finish_package_libs_adjacent  --
   ------------------------------------
   procedure finish_package_libs_adjacent (db         : RDB_Connection;
                                           incomplete : in out Pkgtypes.A_Package)
   is
      new_stmt : SQLite.thick_stmt;
      func : constant String := "finish_package_libs_adjacent";
      sql : constant String :=
        "SELECT x.library_id, ml.name FROM pkg_libs_adjacent x " &
        "JOIN libraries ml ON x.library_id = ml.library_id WHERE x.package_id = ? ORDER BY ml.name";
   begin
      incomplete.libs_adjacent.clear;
      if not SQLite.prepare_sql (db.handle, sql, new_stmt) then
         CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func, sql);
         return;
      end if;
      SQLite.bind_integer (new_stmt, 1, SQLite.sql_int64 (incomplete.id));
      debug_running_stmt (new_stmt);

      loop
         case SQLite.step (new_stmt) is
            when SQLite.no_more_data => exit;
            when SQLite.row_present =>
               incomplete.libs_adjacent.Append (SUS (SQLite.retrieve_string (new_stmt, 1)));
            when SQLite.something_else =>
               CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                            SQLite.get_expanded_sql (new_stmt));
         end case;
      end loop;
      SQLite.finalize_statement (new_stmt);

   end finish_package_libs_adjacent;


   ----------------------------------
   --  finish_package_directories  --
   ----------------------------------
   procedure finish_package_directories (db         : RDB_Connection;
                                         incomplete : in out Pkgtypes.A_Package)
   is
      new_stmt : SQLite.thick_stmt;
      func : constant String := "finish_package_directories";
      sql : constant String :=
        "SELECT x.directory_id, ml.path FROM pkg_directories x " &
        "JOIN directories ml ON x.directory_id = ml.directory_id " &
        "WHERE x.package_id = ? ORDER BY ml.path";
   begin
      incomplete.directories.clear;
      if not SQLite.prepare_sql (db.handle, sql, new_stmt) then
         CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func, sql);
         return;
      end if;
      SQLite.bind_integer (new_stmt, 1, SQLite.sql_int64 (incomplete.id));
      debug_running_stmt (new_stmt);

      loop
         case SQLite.step (new_stmt) is
            when SQLite.no_more_data => exit;
            when SQLite.row_present =>
               incomplete.directories.Append (SUS (SQLite.retrieve_string (new_stmt, 1)));
            when SQLite.something_else =>
               CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                            SQLite.get_expanded_sql (new_stmt));
         end case;
      end loop;
      SQLite.finalize_statement (new_stmt);

   end finish_package_directories;


   -----------------------------------
   --  finish_package_dependencies  --
   -----------------------------------
   procedure finish_package_dependencies
     (db         : RDB_Connection;
      incomplete : in out Pkgtypes.A_Package)
   is
      new_stmt : SQLite.thick_stmt;
      func : constant String := "finish_package_dependencies";
      sql : constant String :=
        "SELECT x.dependency_id, ml.nsv, ml.version FROM pkg_dependencies x " &
        "JOIN dependencies ml ON x.dependency_id = ml.dependency_id " &
        "WHERE x.package_id = ? ORDER BY ml.nsv";
   begin
      incomplete.dependencies.clear;
      if not SQLite.prepare_sql (db.handle, sql, new_stmt) then
         CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func, sql);
         return;
      end if;
      SQLite.bind_integer (new_stmt, 1, SQLite.sql_int64 (incomplete.id));
      debug_running_stmt (new_stmt);

       loop
         case SQLite.step (new_stmt) is
            when SQLite.no_more_data => exit;
            when SQLite.row_present =>
               declare
                  nsv : constant String := SQLite.retrieve_string (new_stmt, 1);
                  ver : constant String := SQLite.retrieve_string (new_stmt, 2);
               begin
                  incomplete.dependencies.Insert (SUS (nsv), SUS (ver));
               end;
            when SQLite.something_else =>
               CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                            SQLite.get_expanded_sql (new_stmt));
         end case;
      end loop;
      SQLite.finalize_statement (new_stmt);
   end finish_package_dependencies;


   -----------------------------------
   --  finish_package_annotations  --
   -----------------------------------
   procedure finish_package_annotations
     (db         : RDB_Connection;
      incomplete : in out Pkgtypes.A_Package)
   is
      new_stmt : SQLite.thick_stmt;
      func : constant String := "finish_package_annotations";
      sql : constant String :=
        "SELECT x.annotation_id, ml.note_key, x.annotation FROM pkg_annotations x " &
        "JOIN annotations ml ON x.annotation_id = ml.annotation_id " &
        "WHERE x.package_id = ? ORDER BY ml.note_key";
   begin
      incomplete.annotations.clear;
      if not SQLite.prepare_sql (db.handle, sql, new_stmt) then
         CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func, sql);
         return;
      end if;
      SQLite.bind_integer (new_stmt, 1, SQLite.sql_int64 (incomplete.id));
      debug_running_stmt (new_stmt);

       loop
         case SQLite.step (new_stmt) is
            when SQLite.no_more_data => exit;
            when SQLite.row_present =>
               declare
                  key : constant String := SQLite.retrieve_string (new_stmt, 1);
                  val : constant String := SQLite.retrieve_string (new_stmt, 2);
               begin
                  incomplete.annotations.Insert (SUS (key), SUS (val));
               end;
            when SQLite.something_else =>
               CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                            SQLite.get_expanded_sql (new_stmt));
         end case;
      end loop;
      SQLite.finalize_statement (new_stmt);
   end finish_package_annotations;


   ------------------------------
   --  finish_package_options  --
   ------------------------------
   procedure finish_package_options
     (db         : RDB_Connection;
      incomplete : in out Pkgtypes.A_Package)
   is
      new_stmt : SQLite.thick_stmt;
      func : constant String := "finish_package_options";
      sql : constant String :=
        "SELECT x.option_id, ml.option_name, x. option_setting FROM pkg_options x " &
        "JOIN options ml ON x.option_id = ml.option_id " &
        "WHERE x.package_id = ? ORDER BY ml.option_name";
   begin
      incomplete.options.clear;
      if not SQLite.prepare_sql (db.handle, sql, new_stmt) then
         CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func, sql);
         return;
      end if;
      SQLite.bind_integer (new_stmt, 1, SQLite.sql_int64 (incomplete.id));
      debug_running_stmt (new_stmt);

      loop
         case SQLite.step (new_stmt) is
            when SQLite.no_more_data => exit;
            when SQLite.row_present =>
               declare
                  key : constant String := SQLite.retrieve_string (new_stmt, 1);
                  val : constant String := SQLite.retrieve_string (new_stmt, 2);
               begin
                  incomplete.options.Insert (SUS (key), SUS (val));
               end;
            when SQLite.something_else =>
               CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                            SQLite.get_expanded_sql (new_stmt));
         end case;
      end loop;
      SQLite.finalize_statement (new_stmt);
   end finish_package_options;


   -------------------------------
   --  finish_package_messages  --
   -------------------------------
   procedure finish_package_messages
     (db         : RDB_Connection;
      incomplete : in out Pkgtypes.A_Package)
   is
      new_stmt : SQLite.thick_stmt;
      func : constant String := "finish_package_messages";
      sql : constant String :=
        "SELECT message_index, message_type, message, min_version, max_version " &
        "FROM pkg_messages where package_id = ? ORDER BY message_index";
   begin
      for mtype in Pkgtypes.Message_Type loop
         incomplete.messages (mtype).Clear;
      end loop;
      if not SQLite.prepare_sql (db.handle, sql, new_stmt) then
         CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func, sql);
         return;
      end if;
      SQLite.bind_integer (new_stmt, 1, SQLite.sql_int64 (incomplete.id));
      debug_running_stmt (new_stmt);

      loop
         case SQLite.step (new_stmt) is
            when SQLite.no_more_data => exit;
            when SQLite.row_present =>
               declare
                  myrec : Pkgtypes.Message_Parameters;
                  mtype : Pkgtypes.Message_Type;
               begin
                  myrec.message         := SUS (SQLite.retrieve_string (new_stmt, 2));
                  myrec.minimum_version := SUS (SQLite.retrieve_string (new_stmt, 3));
                  myrec.maximum_version := SUS (SQLite.retrieve_string (new_stmt, 4));
                  mtype := Pkgtypes.Message_Type'Val (SQLite.retrieve_integer (new_stmt, 1));
                  incomplete.messages (mtype).Append (myrec);
               end;
            when SQLite.something_else =>
               CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                            SQLite.get_expanded_sql (new_stmt));
         end case;
      end loop;
      SQLite.finalize_statement (new_stmt);
   end finish_package_messages;


   ------------------------------
   --  finish_package_scripts  --
   ------------------------------
   procedure finish_package_scripts
     (db         : RDB_Connection;
      incomplete : in out Pkgtypes.A_Package)
   is
      new_stmt : SQLite.thick_stmt;
      func : constant String := "finish_package_scripts";
      sql : constant String :=
        "SELECT x.script_id, ml.code, x.script_type, x.arguments " &
        "FROM pkg_scripts x " &
        "JOIN scripts ml ON x.script_id = ml.script_id " &
        "WHERE x.package_id = ? ORDER BY x.script_id, x.script_type, x.type_index";
   begin
      for ptype in Pkgtypes.ARW.package_phase loop
         incomplete.scripts (ptype).Clear;
      end loop;
      if not SQLite.prepare_sql (db.handle, sql, new_stmt) then
         CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func, sql);
         return;
      end if;
      SQLite.bind_integer (new_stmt, 1, SQLite.sql_int64 (incomplete.id));
      debug_running_stmt (new_stmt);

      loop
         case SQLite.step (new_stmt) is
            when SQLite.no_more_data => exit;
            when SQLite.row_present =>
               declare
                  myrec : Pkgtypes.Script_Parameters;
                  ptype : Pkgtypes.ARW.package_phase;
               begin
                  myrec.args := SUS (SQLite.retrieve_string (new_stmt, 3));
                  myrec.code := SUS (SQLite.retrieve_string (new_stmt, 1));
                  ptype  := Pkgtypes.ARW.package_phase'Val (SQLite.retrieve_integer (new_stmt, 2));
                  incomplete.scripts (ptype).Append (myrec);
               end;
            when SQLite.something_else =>
               CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                            SQLite.get_expanded_sql (new_stmt));
         end case;
      end loop;
      SQLite.finalize_statement (new_stmt);
   end finish_package_scripts;


   ----------------------------
   --  finish_package_files  --
   ----------------------------
   procedure finish_package_files
     (db         : RDB_Connection;
      incomplete : in out Pkgtypes.A_Package)
   is
      new_stmt : SQLite.thick_stmt;
      func : constant String := "finish_package_messages";
      sql : constant String := "SELECT path, b3digest FROM pkg_files where package_id = ?";
      --  do not sort.  There could be thousands of files and this table is not indexed
   begin
      incomplete.files.Clear;
      if not SQLite.prepare_sql (db.handle, sql, new_stmt) then
         CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func, sql);
         return;
      end if;
      SQLite.bind_integer (new_stmt, 1, SQLite.sql_int64 (incomplete.id));
      debug_running_stmt (new_stmt);

      loop
         case SQLite.step (new_stmt) is
            when SQLite.no_more_data => exit;
            when SQLite.row_present =>
               declare
                  myrec : Pkgtypes.File_Item;
               begin
                  myrec.path   := SUS (SQLite.retrieve_string (new_stmt, 0));
                  myrec.digest := Blake_3.blake3_hash_hex (SQLite.retrieve_string (new_stmt, 1));
                  incomplete.files.Append (myrec);
               end;
            when SQLite.something_else =>
               CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                            SQLite.get_expanded_sql (new_stmt));
         end case;
      end loop;
      SQLite.finalize_statement (new_stmt);

   end finish_package_files;


   ----------------------
   --  finish_package  --
   ----------------------
   procedure finish_package
     (db         : RDB_Connection;
      incomplete : in out Pkgtypes.A_Package)
   is
   begin
      finish_package_users         (db, incomplete);
      finish_package_groups        (db, incomplete);
      finish_package_licenses      (db, incomplete);
      finish_package_categories    (db, incomplete);
      finish_package_libs_required (db, incomplete);
      finish_package_libs_provided (db, incomplete);
      finish_package_libs_adjacent (db, incomplete);
      finish_package_categories    (db, incomplete);
      finish_package_dependencies  (db, incomplete);
      finish_package_annotations   (db, incomplete);
      finish_package_annotations   (db, incomplete);
      finish_package_options       (db, incomplete);
      finish_package_messages      (db, incomplete);
      finish_package_scripts       (db, incomplete);
      finish_package_files         (db, incomplete);
   end finish_package;

end Raven.Database.Query;
