--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Text_IO;
with Raven.Pkgtypes;
with Raven.Database;
with ThickUCL;

private with Ada.Containers.Vectors;

package Raven.Install is

   --  current_pkg only needs to be an unfinished package + annotations.
   --  This is make the automatic flag and the custom annotations persistent across
   --  upgrades and reinstallations.  rdb must be open and exclusively locked.
   --  rootdir comes from the pre-command install_rootdir value
   --
   --  Step 1. extract metadata from rvn package
   --  Step 2. convert updated_pkg to A_Package structure
   --  Step 3. Overwrite automatic and add any custom annotations
   --  Step 4. Remove current installation (record and files)
   --  Step 5. Extract rvn package
   --  Step 6. Register package

   type refresh_action is (new_install, reinstall, upgrade, reset_auto);

   --  Prior to this routine many things need to have been done:
   --  * archive_path verified to be an existing regular file
   --  * conflict check (conflicts overridden by --forced)
   --  * interactive user confirmations
   --  * plan to handle dependencies (affected by --ignore-missing and --recursive)
   --  * This routine populates the file list.
   --  This routine installs the files from the RVN archive into the root directory
   --    (affected by rvn -[r|c]).   If dry-run selected no installation will actually occur.
   --  Returns true if extraction was successful.
   function install_files_from_archive
     (archive_path      : String;
      inhibit_scripts   : Boolean;
      be_silent         : Boolean;
      dry_run_only      : Boolean;
      upgrading         : Boolean;
      rootdir           : String;
      package_data      : Pkgtypes.A_Package;
      post_report       : Ada.Text_IO.File_Type) return Boolean;

   function install_remote_packages
     (opt_exact_match  : Boolean;
      opt_quiet        : Boolean;
      opt_automatic    : Boolean;
      opt_manual       : Boolean;
      opt_drop_depends : Boolean;
      opt_force        : Boolean;
      opt_skip_scripts : Boolean;
      opt_dry_run      : Boolean;
      opt_fetch_only   : Boolean;
      single_repo      : String;
      rootdir          : String;
      patterns         : Pkgtypes.Text_List.Vector)
      return Boolean;

   function upgrade_installed_packages
     (opt_exact_match  : Boolean;
      opt_quiet        : Boolean;
      opt_force        : Boolean;
      opt_skip_scripts : Boolean;
      opt_dry_run      : Boolean;
      opt_fetch_only   : Boolean;
      single_repo      : String;
      rootdir          : String;
      patterns         : Pkgtypes.Text_List.Vector)
      return Boolean;

private

   type Install_Order_Type is
      record
         action          : refresh_action := new_install;
         prov_lib_change : Boolean := False;
         automatic       : Boolean := False;
         level           : Natural := 0;
         nsv             : text := SU.Null_Unbounded_String;
      end record;

   package Install_Order_Set is new Ada.Containers.Vectors
     (Element_Type => Install_Order_Type,
      Index_Type   => Natural);

   type Descendant_Type is
      record
         descendents     : Natural := 0;
         nsv             : text := SU.Null_Unbounded_String;
      end record;
   function desc_desc (left, right : Descendant_Type) return Boolean;

   package Descendant_Set is new Ada.Containers.Vectors
     (Element_Type => Descendant_Type,
      Index_Type   => Natural);

   package Desc_sorter is new Descendant_Set.Generic_Sorting ("<" => desc_desc);

   function install_or_upgrade
     (rdb         : in out Database.RDB_Connection;
      action      : refresh_action;
      current_pkg : Pkgtypes.A_Package;
      updated_pkg : String;
      no_scripts  : Boolean;
      rootdir     : String;
      post_report : Ada.Text_IO.File_Type) return Boolean;

   function assemble_work_queue
     (rdb             : in out Database.RDB_Connection;
      opt_exact_match : Boolean;
      patterns        : Pkgtypes.Text_List.Vector;
      toplevel        : in out Pkgtypes.Package_Map.Map) return Boolean;

   procedure calculate_descendants
     (rdb         : in out Database.RDB_Connection;
      catalog_map : Pkgtypes.Package_Map.Map;
      cache_map   : in out Pkgtypes.Package_Map.Map;
      priority    : in out Descendant_Set.Vector;
      skip_depend : Boolean;
      from_local  : Boolean;
      missing_dep : out Boolean);

   procedure load_installation_data
     (localdb     : Database.RDB_Connection;
      cache_map   : Pkgtypes.Package_Map.Map;
      install_map : in out Pkgtypes.Package_Map.Map);

   procedure finalize_work_queue
     (localdb       : Database.RDB_Connection;
      install_map   : Pkgtypes.Package_Map.Map;
      cache_map     : Pkgtypes.Package_Map.Map;
      priority      : Descendant_Set.Vector;
      opt_automatic : Boolean;
      opt_manual    : Boolean;
      opt_exactly   : Boolean;
      opt_force     : Boolean;
      opt_drop_deps : Boolean;
      opt_noscripts : Boolean;
      queue         : in out Install_Order_Set.Vector);

   procedure calc_dependency_descendants
     (depend_set : Pkgtypes.NV_Pairs.Map;
      cache_map  : Pkgtypes.Package_Map.Map;
      priority   : in out Descendant_Set.Vector);

   function conflict_free
     (queue           : Install_Order_Set.Vector;
      cache_map       : Pkgtypes.Package_Map.Map;
      file_collection : in out Pkgtypes.NV_Pairs.Map) return Boolean;

   procedure print_next_installation
     (nextpkg  : Install_Order_Type;
      version  : String;
      counter  : Natural;
      total    : Natural);

   function granted_permission_to_proceed
     (quiet : Boolean) return Boolean;

   procedure show_proposed_queue
     (queue        : Install_Order_Set.Vector;
      cache_map    : Pkgtypes.Package_Map.Map;
      install_map  : Pkgtypes.Package_Map.Map;
      behave_quiet : Boolean;
      only_dryrun  : Boolean);

   function execute_installation_queue
     (rdb          : in out Database.RDB_Connection;
      queue        : Install_Order_Set.Vector;
      cache_map    : Pkgtypes.Package_Map.Map;
      install_map  : Pkgtypes.Package_Map.Map;
      skip_scripts : Boolean;
      behave_quiet : Boolean;
      rootdir      : String) return Boolean;

   --  queue represents the most important top-level ports branching to their dependencies.
   --  for building, we need to install the dependencies first.  reverse_queue is not simply
   --  reversing the order of the queue.  It is the order or recursively iterating over the
   --  dependencies in order of priority, returning once the dependencies are depleted.
   procedure build_reverse_queue
     (queue        : Install_Order_Set.Vector;
      rev_queue    : in out Install_Order_Set.Vector);

   --  Only the metadata contains the permissions and ownership of the empty
   --  directories to create upon extraction
   procedure create_standalone_directories
     (package_metadata : ThickUCL.UclTree;
      num_directories  : Natural;
      extract_location : String);

end Raven.Install;
