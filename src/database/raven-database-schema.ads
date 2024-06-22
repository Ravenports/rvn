--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Strings; use Raven.Strings;

package Raven.Database.Schema is

   type schema_component is
     (version, packages,
      users, groups, scripts, options, licenses, categories,
      directories, annotations, dependencies, libraries,
      pkg_users, pkg_groups, pkg_scripts, pkg_options, pkg_licences, pkg_categories,
      pkg_directories, pkg_annotations, pkg_dependencies, pkg_messages, pkg_libs_provided,
      pkg_libs_required, pkg_libs_adacent, pkg_files, pkg_triggers, trigger_paths,
      lock_state, lock_process,
      index_dep1, index_dep2, index_dep3, index_files, index_pkg_namebase, index_pkg_nsv
     );

   type prepared_statement is
     (main_pkg, user, pkg_user, group, pkg_group, script, pkg_script, option, pkg_option,
      license, pkg_license, category, pkg_category, directory, pkg_directory, note, pkg_note,
      dependency, pkg_dependency, library, pkg_provided_lib, pkg_required_lib, pkg_adjacent_lib,
      pkg_file, pkg_message, pkg_trigger, trig_paths);

   function component_definition (component : schema_component) return String;
   function prstat_definition (component : prepared_statement) return String;
   function upgrade_definition (component : Local_Upgrade_Series) return String;

private

   user_id       : constant String := "user_id";
   group_id      : constant String := "group_id";
   script_id     : constant String := "script_id";
   option_id     : constant String := "option_id";
   package_id    : constant String := "package_id";
   license_id    : constant String := "license_id";
   library_id    : constant String := "library_id";
   trigger_id    : constant String := "trigger_id";
   category_id   : constant String := "category_id";
   directory_id  : constant String := "directory_id";
   annotation_id : constant String := "annotation_id";
   dependency_id : constant String := "dependency_id";

   function table_packages          return String;
   function table_users             return String;
   function table_groups            return String;
   function table_scripts           return String;
   function table_options           return String;
   function table_licenses          return String;
   function table_categories        return String;
   function table_directories       return String;
   function table_annotations       return String;
   function table_dependencies      return String;
   function table_libraries         return String;
   function table_trigger_paths     return String;

   function table_pkg_files         return String;
   function pragma_version          return String;

   function table_pkg_users         return String;
   function table_pkg_groups        return String;
   function table_pkg_scripts       return String;
   function table_pkg_options       return String;
   function table_pkg_licenses      return String;
   function table_pkg_messages      return String;
   function table_pkg_categories    return String;
   function table_pkg_directories   return String;
   function table_pkg_annotations   return String;
   function table_pkg_dependencies  return String;
   function table_pkg_libs_provided return String;
   function table_pkg_libs_required return String;
   function table_pkg_libs_adjacent return String;
   function table_pkg_triggers      return String;
   function table_lock_state        return String;
   function table_lock_process      return String;

   function index_dependencies_1    return String;
   function index_dependencies_2    return String;
   function index_dependencies_3    return String;
   function index_files             return String;
   function index_pkg_namebase      return String;
   function index_pkg_nsv           return String;

   function start_table (table_name : String) return Text;
   function close_table      (def : in out Text) return String;
   procedure prime_key       (def : in out Text; name : String);
   procedure prime_key_text  (def : in out Text; name : String);
   procedure col_text_unique (def : in out Text; name : String);
   procedure col_text        (def : in out Text; name : String; required : Boolean := True);
   procedure col_int         (def : in out Text; name : String; required : Boolean := True);
   procedure col_boolean     (def : in out Text; name : String);
   procedure multi_primekey2 (def : in out Text; part1, part2 : String);
   procedure multi_primekey3 (def : in out Text; part1, part2, part3 : String);

   function generic_index    (index_name, table_name, table_column : String) return String;

   --  <name> INTEGER REFERENCES <ref_table>(<ref_column>) ON DELETE CASCADE ON UPDATE CASCADE
   procedure cascade
     (def       : in out Text;
      name      : String;
      ref_table : String;
      ref_column : String);

   --  <name> INTEGER REFERENCES <ref_table>(<ref_column>) ON DELETE RESTRICT ON UPDATE RESTRICT,
   procedure restrict
     (def       : in out Text;
      name      : String;
      ref_table : String;
      ref_column : String);

end Raven.Database.Schema;
