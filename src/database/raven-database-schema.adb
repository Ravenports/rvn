--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

package body Raven.Database.Schema is

   ----------------------------
   --  component_definition  --
   ----------------------------
   function component_definition (component : schema_component) return String is
   begin
      case component is
         when packages           => return table_packages;
         when users              => return table_users;
         when groups             => return table_groups;
         when scripts            => return table_scripts;
         when options            => return table_options;
         when licenses           => return table_licenses;
         when categories         => return table_categories;
         when directories        => return table_directories;
         when annotations        => return table_annotations;
         when dependencies       => return table_dependencies;
         when libraries          => return table_libraries;
         when pkg_users          => return table_pkg_users;
         when pkg_groups         => return table_pkg_groups;
         when pkg_scripts        => return table_pkg_scripts;
         when pkg_options        => return table_pkg_options;
         when pkg_licences       => return table_pkg_licenses;
         when pkg_messages       => return table_pkg_messages;
         when pkg_categories     => return table_pkg_categories;
         when pkg_directories    => return table_pkg_directories;
         when pkg_annotations    => return table_pkg_annotations;
         when pkg_dependencies   => return table_pkg_dependencies;
         when pkg_libs_provided  => return table_pkg_libs_provided;
         when pkg_libs_required  => return table_pkg_libs_required;
         when pkg_libs_adacent   => return table_pkg_libs_adjacent;
         when pkg_files          => return table_pkg_files;
         when pkg_triggers       => return table_pkg_triggers;
         when index_dep1         => return index_dependencies_1;
         when index_dep2         => return index_dependencies_2;
         when index_dep3         => return index_dependencies_3;
         when index_files        => return index_files;
         when index_pkg_namebase => return index_pkg_namebase;
         when index_pkg_nsv      => return index_pkg_nsv;
         when version            => return pragma_version;
         when lock_state         => return table_lock_state;
         when lock_process       => return table_lock_process;
         when trigger_paths      => return table_trigger_paths;
      end case;
   end component_definition;


   -----------------------------
   --  table_pkg_dependencies --
   -----------------------------
   function table_pkg_dependencies return String
   is
      def : Text := start_table ("pkg_dependencies");
   begin
      cascade  (def, package_id, "packages", "id");
      restrict (def, dependency_id, "dependencies", dependency_id);
      multi_primekey2 (def, package_id, dependency_id);
      return close_table (def);
   end table_pkg_dependencies;


   ------------------------
   --  table_pkg_options --
   ------------------------
   function table_pkg_options return String
   is
      def : Text := start_table ("pkg_options");
   begin
      cascade  (def, package_id, "packages", "id");
      restrict (def, option_id, "options", option_id);
      col_text (def, "option_setting");
      multi_primekey2 (def, package_id, option_id);
      return close_table (def);
   end table_pkg_options;


   -------------------------------
   --  table_pkg_libs_adjacent  --
   -------------------------------
   function table_pkg_libs_adjacent return String
   is
      def : Text := start_table ("pkg_libs_adjacent");
   begin
      cascade  (def, package_id, "packages", "id");
      restrict (def, library_id, "libraries", library_id);
      multi_primekey2 (def, package_id, library_id);
      return close_table (def);
   end table_pkg_libs_adjacent;


   -------------------------------
   --  table_pkg_libs_required  --
   -------------------------------
   function table_pkg_libs_required return String
   is
      def : Text := start_table ("pkg_libs_required");
   begin
      cascade  (def, package_id, "packages", "id");
      restrict (def, library_id, "libraries", library_id);
      multi_primekey2 (def, package_id, library_id);
      return close_table (def);
   end table_pkg_libs_required;


   -------------------------------
   --  table_pkg_libs_provided  --
   -------------------------------
   function table_pkg_libs_provided return String
   is
      def : Text := start_table ("pkg_libs_provided");
   begin
      cascade  (def, package_id, "packages", "id");
      restrict (def, library_id, "libraries", library_id);
      multi_primekey2 (def, package_id, library_id);
      return close_table (def);
   end table_pkg_libs_provided;


   -----------------------------
   --  table_pkg_annotations  --
   -----------------------------
   function table_pkg_annotations return String
   is
      def : Text := start_table ("pkg_annotations");
   begin
      cascade  (def, package_id, "packages", "id");
      restrict (def, annotation_id, "annotations", annotation_id);
      col_text (def, "annotation");
      multi_primekey2 (def, package_id, annotation_id);
      return close_table (def);
   end table_pkg_annotations;


   -----------------------------
   --  table_pkg_directories  --
   -----------------------------
   function table_pkg_directories return String
   is
      def : Text := start_table ("pkg_directories");
   begin
      cascade  (def, package_id, "packages", "id");
      restrict (def, directory_id, "directories", directory_id);
      multi_primekey2 (def, package_id, directory_id);
      return close_table (def);
   end table_pkg_directories;


   --------------------------
   --  table_pkg_licenses  --
   --------------------------
   function table_pkg_licenses return String
   is
      def : Text := start_table ("pkg_licenses");
   begin
      cascade  (def, package_id, "packages", "id");
      restrict (def, license_id, "licenses", license_id);
      multi_primekey2 (def, package_id, license_id);
      return close_table (def);
   end table_pkg_licenses;


   ------------------------
   --  table_pkg_groups  --
   ------------------------
   function table_pkg_groups return String
   is
      def : Text := start_table ("pkg_groups");
   begin
      cascade  (def, package_id, "packages", "id");
      restrict (def, group_id, "groups", group_id);
      multi_primekey2 (def, package_id, group_id);
      return close_table (def);
   end table_pkg_groups;


   -----------------------
   --  table_pkg_users  --
   -----------------------
   function table_pkg_users return String
   is
      def : Text := start_table ("pkg_users");
   begin
      cascade  (def, package_id, "packages", "id");
      restrict (def, user_id, "users", user_id);
      multi_primekey2 (def, package_id, user_id);
      return close_table (def);
   end table_pkg_users;


   ----------------------------
   --  table_pkg_categories  --
   ----------------------------
   function table_pkg_categories return String
   is
      def : Text := start_table ("pkg_categories");
   begin
      cascade  (def, package_id, "packages", "id");
      restrict (def, category_id, "categories", category_id);
      multi_primekey2 (def, package_id, category_id);
      return close_table (def);
   end table_pkg_categories;


   --------------------------
   --  table_pkg_triggers  --
   --------------------------
   function table_pkg_triggers return String
   is
      def : Text := start_table ("pkg_triggers");
   begin
      prime_key (def, trigger_id);
      cascade (def, package_id, "packages", "id");
      col_int (def, "trigger_type", True);
      col_text (def, "code", True);
      return close_table (def);
   end table_pkg_triggers;


   ---------------------------
   --  table_trigger_paths  --
   ---------------------------
   function table_trigger_paths return String
   is
      def : Text := start_table ("trigger_paths");
      path_type : constant String := "path_type";
      type_index  : constant String := "type_index";
   begin
      cascade  (def, trigger_id, "pkg_triggers", trigger_id);
      col_int (def, path_type, True);
      col_int (def, type_index, True);
      col_text (def, "path_value", True);
      multi_primekey3 (def, trigger_id, path_type, type_index);
      return close_table (def);
   end table_trigger_paths;


   --------------------------
   --  table_dependencies  --
   --------------------------
   function table_dependencies return String
   is
       def : Text := start_table ("dependencies");
   begin
      prime_key (def, dependency_id);
      col_text (def, "nsv");
      col_text (def, "version");
      return close_table (def);
   end table_dependencies;


   ----------------------------
   --  index_dependencies_1  --
   ----------------------------
   function index_dependencies_1 return String is
   begin
      return generic_index ("dep_pkg_id", "dependencies", dependency_id);
   end index_dependencies_1;


   ----------------------------
   --  index_dependencies_2  --
   ----------------------------
   function index_dependencies_2 return String is
   begin
      return generic_index ("dep_namebase", "dependencies", "nsv");
   end index_dependencies_2;


   ----------------------------
   --  index_dependencies_3  --
   ----------------------------
   function index_dependencies_3 return String is
   begin
      return "CREATE UNIQUE INDEX dep_unique ON dependencies(nsv, version);";
   end index_dependencies_3;


   -----------------------
   --  table_libraries  --
   -----------------------
   function table_libraries return String
   is
       def : Text := start_table ("libraries");
   begin
      prime_key (def, library_id);
      col_text_unique (def, "name");
      return close_table (def);
   end table_libraries;


   -------------------------
   --  table_pkg_scripts  --
   -------------------------
   function table_pkg_scripts return String
   is
      def : Text := start_table ("pkg_scripts");
      script_type : constant String := "script_type";
      type_index  : constant String := "type_index";
   begin
      cascade  (def, package_id, "packages", "id");
      col_int  (def, script_type);
      col_int  (def, type_index);
      restrict (def, script_id, "scripts", script_id);
      col_text (def, "arguments");
      multi_primekey3 (def, package_id, script_type, type_index);
      return close_table (def);
   end table_pkg_scripts;


   ---------------------
   --  table_scripts  --
   ---------------------
   function table_scripts return String
   is
       def : Text := start_table ("scripts");
   begin
      prime_key (def, script_id);
      col_text_unique (def, "code");
      return close_table (def);
   end table_scripts;


   ---------------------
   --  table_options  --
   ---------------------
   function table_options return String
   is
       def : Text := start_table ("options");
   begin
      prime_key (def, option_id);
      col_text (def, "option_name");
      return close_table (def);
   end table_options;


   ----------------------
   --  table_licenses  --
   ----------------------
   function table_licenses return String
   is
       def : Text := start_table ("licenses");
   begin
      prime_key (def, license_id);
      col_text_unique (def, "name");
      return close_table (def);
   end table_licenses;


   -------------------------
   --  table_annotations  --
   -------------------------
   function table_annotations return String
   is
       def : Text := start_table ("annotations");
   begin
      prime_key (def, annotation_id);
      col_text_unique (def, "note_key");
      return close_table (def);
   end table_annotations;


   -------------------------
   --  table_directories  --
   -------------------------
   function table_directories return String
   is
      def : Text := start_table ("directories");
   begin
      prime_key (def, directory_id);
      col_text_unique (def, "path");
      return close_table (def);
   end table_directories;


   ------------------------
   --  table_categories  --
   ------------------------
   function table_categories return String
   is
       def : Text := start_table ("categories");
   begin
      prime_key (def, category_id);
      col_text_unique (def, "name");
      return close_table (def);
   end table_categories;


   -------------------
   --  table_users  --
   -------------------
   function table_users return String
   is
       def : Text := start_table ("users");
   begin
      prime_key (def, user_id);
      col_text_unique (def, "name");
      return close_table (def);
   end table_users;


   --------------------
   --  table_groups  --
   --------------------
   function table_groups return String
   is
       def : Text := start_table ("groups");
   begin
      prime_key (def, group_id);
      col_text_unique (def, "name");
      return close_table (def);
   end table_groups;


   -----------------------
   --  table_pkg_files  --
   -----------------------
   function table_pkg_files return String
   is
      def : Text := start_table ("pkg_files");
   begin
      prime_key_text (def, "path");
      col_text (def, "b3digest");
      cascade (def, package_id, "packages", "id");
      return close_table (def);
   end table_pkg_files;


   --------------------------
   --  table_pkg_messages  --
   --------------------------
   function table_pkg_messages return String
   is
      --  message_type enumerations
      --  0 => install
      --  1 => remove
      --  2 => upgrade
      def : Text := start_table ("pkg_messages");
      message_type : constant String := "message_type";
      message_index  : constant String := "message_index";
   begin
      cascade  (def, package_id, "packages", "id");
      col_int  (def, message_index);
      col_int  (def, message_type);
      col_text (def, "message");
      col_text (def, "min_version");
      col_text (def, "max_version");
      multi_primekey2 (def, package_id, message_index);
      return close_table (def);
   end table_pkg_messages;


   -------------------
   --  index_files  --
   -------------------
   function index_files return String is
   begin
      return generic_index ("files_pkg_id", "pkg_files", package_id);
   end index_files;


   ----------------------
   --  table_packages  --
   ----------------------
   function table_packages return String
   is
      def : Text := start_table ("packages");
   begin
      prime_key (def, "id");
      col_text (def, "namebase");
      col_text (def, "subpackage");
      col_text (def, "variant");
      col_text (def, "version");
      col_text (def, "comment");
      col_text (def, "desc");
      col_text (def, "www", required => False);
      col_text (def, "maintainer");
      col_text (def, "prefix");
      col_text (def, "abi");
      col_text (def, "rvndigest");
      col_int  (def, "rvnsize");
      col_int  (def, "flatsize");
      col_int  (def, "licenselogic");
      col_int  (def, "automatic");
      col_int  (def, "installed");
      return close_table (def);
   end table_packages;


   ------------------------
   --  table_lock_state  --
   ------------------------
   function table_lock_state return String
   is
      def : Text := start_table ("lock_state");
   begin
      col_boolean (def, "exclusive");
      col_boolean (def, "advisory");
      col_int     (def, "read");
      return close_table (def);
   end table_lock_state;


   --------------------------
   --  table_lock_process  --
   --------------------------
   function table_lock_process return String
   is
      def : Text := start_table ("lock_process");
   begin
      prime_key (def, "pid");
      return close_table (def);
   end table_lock_process;


   --------------------------
   --  index_pkg_namebase  --
   --------------------------
   function index_pkg_namebase return String is
   begin
      return generic_index ("packages_namebase", "packages", "namebase COLLATE NOCASE");
   end index_pkg_namebase;


   ---------------------
   --  index_pkg_nsv  --
   ---------------------
   function index_pkg_nsv return String is
   begin
      return "CREATE UNIQUE INDEX packages_nsv ON packages(namebase, subpackage, variant);";
   end index_pkg_nsv;


   ---------------------=
   --  pragma_version  --
   ---------------------=
   function pragma_version return String is
   begin
      return "PRAGMA user_version = 0;";
   end pragma_version;


   -------------------
   --  start_table  --
   -------------------
   function start_table (table_name : String) return Text is
   begin
      return SUS ("CREATE TABLE " & table_name & " (");
   end start_table;


   -----------------
   --  prime_key  --
   -----------------
   procedure prime_key (def : in out Text; name : String) is
   begin
      SU.Append (def, name & " INTEGER PRIMARY KEY, ");
   end prime_key;


   ----------------------
   --  prime_key_text  --
   ----------------------
   procedure prime_key_text (def : in out Text; name : String) is
   begin
      SU.Append (def, name & " TEXT PRIMARY KEY, ");
   end prime_key_text;


   ----------------
   --  col_text  --
   ----------------
   procedure col_text (def : in out Text; name : String;  required : Boolean := True) is
   begin
      if required then
         SU.Append (def, name & " TEXT NOT NULL, ");
      else
         SU.Append (def, name & " TEXT, ");
      end if;
   end col_text;


   -----------------------
   --  col_text_unique  --
   -----------------------
   procedure col_text_unique (def : in out Text; name : String) is
   begin
      SU.Append (def, name & " TEXT NOT NULL UNIQUE, ");
   end col_text_unique;


   ---------------
   --  col_int  --
   ---------------
   procedure col_int (def : in out Text; name : String; required : Boolean := True) is
   begin
      if required then
         SU.Append (def, name & " INTEGER NOT NULL, ");
      else
         SU.Append (def, name & " INTEGER, ");
      end if;
   end col_int;


   -------------------
   --  col_boolean  --
   -------------------
   procedure col_boolean (def : in out Text; name : String) is
   begin
      SU.Append (def, name & " INTEGER(1), ");
   end col_boolean;


   -------------------
   --  close_table  --
   -------------------
   function close_table (def : in out Text) return String
   is
      --  replace last two characters (", ") with ");"
      query : String := USS (def);
   begin
      query (query'Last - 1 .. query'Last) := ");";
      return query;
   end close_table;


   ---------------
   --  cascade  --
   ---------------
   procedure cascade
     (def       : in out Text;
      name      : String;
      ref_table : String;
      ref_column : String) is
   begin
      SU.Append (def, name & " INTEGER REFERENCES " & ref_table & "(" & ref_column
                 & ") ON DELETE CASCADE ON UPDATE CASCADE, ");
   end cascade;


   ----------------
   --  restrict  --
   ----------------
   procedure restrict
     (def       : in out Text;
      name      : String;
      ref_table : String;
      ref_column : String) is
   begin
      SU.Append (def, name & " INTEGER REFERENCES " & ref_table & "(" & ref_column
                 & ") ON DELETE RESTRICT ON UPDATE RESTRICT, ");
   end restrict;


   -----------------------
   --  multi_primekey2  --
   -----------------------
   procedure multi_primekey2 (def : in out Text; part1, part2 : String) is
   begin
      SU.Append (def, " PRIMARY KEY (" & part1 & ", " & part2 & "), ");
   end multi_primekey2;


   -----------------------
   --  multi_primekey3  --
   -----------------------
   procedure multi_primekey3 (def : in out Text; part1, part2, part3 : String) is
   begin
      SU.Append (def, " PRIMARY KEY (" & part1 & ", " & part2 & ", " & part3 & "), ");
   end multi_primekey3;


   ---------------------
   --  generic_index  --
   ---------------------
   function generic_index (index_name, table_name, table_column : String) return String is
   begin
      return "CREATE INDEX " & index_name & " ON " & table_name & "(" & table_column & ");";
   end generic_index;


   -------------------------
   --  prstat_definition  --
   -------------------------
   function prstat_definition (component : prepared_statement) return String
   is
      IOII : constant String := "INSERT OR IGNORE INTO ";
      IORB : constant String := "INSERT INTO ";  --  was "INSERT OR ROLLBACK INTO"
   begin
      case component is
         when user        => return IOII & "users(name) VALUES(?1)";  --  T
         when group       => return IOII & "groups(name) VALUES(?1)";  --  T
         when script      => return IOII & "scripts(code) VALUES(?1)";  -- T
         when license     => return IOII & "licenses(name) VALUES(?1)";  --  T
         when library     => return IOII & "libraries(name) VALUES(?1)";  --  T
         when category    => return IOII & "categories(name) VALUES(?1)";  --  T
         when directory   => return IOII & "directories(path) VALUES(?1)";  --  T
         when note        => return IOII & "annotations(note_key) VALUES(?1)";  --  T
         when option      => return IOII & "options(option_name) VALUES(?1)";  --  T
         when dependency  => return IOII & "dependencies(nsv,version) VALUES(?1,?2)";  -- TT
         when pkg_file    => return IORB & "pkg_files(package_id,path,b3digest) VALUES(?1,?2,?3);";
         when pkg_user    => return IORB & "pkg_users(package_id, user_id) VALUES" &
                                    "(?1, (SELECT user_id FROM users WHERE name = ?2))";  -- IT
         when pkg_group   => return IORB & "pkg_groups(package_id, group_id) VALUES" &
                                    "(?1, (SELECT group_id FROM groups WHERE name = ?2))";  -- IT
         when pkg_script  => return IORB & "pkg_scripts(package_id,script_type,type_index," &
                                    "arguments,script_id) VALUES(?1,?2,?3,?4," &
                                     "(SELECT script_id FROM scripts WHERE code = ?5))"; --  IIITT
         when pkg_message => return IORB & "pkg_messages(package_id,message_index,message_type," &
                                    "message,min_version,max_version) VALUES (?1,?2,?3,?4,?5,?6)";
                                    --  I,I,I,T,T,T
         when pkg_license => return IORB & "pkg_licenses(package_id, license_id) " &
              "VALUES (?1, (SELECT license_id FROM licenses WHERE name = ?2))";  -- IT
         when pkg_provided_lib => return IORB & "pkg_libs_provided(package_id,library_id) " &
              "VALUES(?1, (SELECT library_id FROM libraries WHERE name = ?2))";  -- IT
         when pkg_required_lib => return IORB & "pkg_libs_required(package_id,library_id) " &
              "VALUES(?1, (SELECT library_id FROM libraries WHERE name = ?2))";  -- IT
         when pkg_adjacent_lib => return IORB & "pkg_libs_adjacent(package_id,library_id) " &
              "VALUES(?1, (SELECT library_id FROM libraries WHERE name = ?2))";  -- IT
         when pkg_category     => return IORB & "pkg_categories(package_id,category_id) " &
              "VALUES(?1, (SELECT category_id FROM categories WHERE name = ?2))";  -- IT
         when pkg_directory    => return IORB & "pkg_directories(package_id,directory_id) " &
              "VALUES(?1, (SELECT directory_id FROM directories WHERE path = ?2))";  -- IT
         when pkg_note         => return IORB &
              "pkg_annotations(package_id,annotation_id,annotation,custom) " &
              "VALUES(?1,(SELECT annotation_id FROM annotations WHERE note_key = ?2),?3,?4)";
            --ITTI
         when pkg_option       => return IORB &
              "pkg_options(package_id,option_id,option_setting) " &
              "VALUES(?1,(SELECT option_id FROM options WHERE option_name = ?2),?3)";  --IIT
         when pkg_dependency   => return IORB & "pkg_dependencies(package_id,dependency_id) VALUES"
              & "(?1, (SELECT dependency_id FROM dependencies WHERE nsv = ?2 AND version = ?3))";
         when main_pkg => return "INSERT OR REPLACE INTO packages(namebase,subpackage,variant," &
              "version,comment,desc,www,maintainer,prefix,abi,rvndigest,rvnsize,flatsize," &
              "licenselogic,automatic,installed) " &
              "VALUES(?1,?2,?3,?4,?5,?6,?7,?8,?9,?10,?11,?12,?13,?14,?15,?16)";
            --TTTT TTTT TTTI IIII
      end case;
   end prstat_definition;


   --------------------------
   --  upgrade_definition  --
   --------------------------
   function upgrade_definition (component : Local_Upgrade_Series) return String is
   begin
      case component is
         when 1 => return "INSERT INTO lock_state VALUES(0,0,0);";
         when 2 => return "ALTER TABLE pkg_annotations ADD custom INTEGER NOT NULL DEFAULT (0);";
      end case;
   end upgrade_definition;

end Raven.Database.Schema;
