--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Pkgtypes;
with Archive.Unpack;

package Raven.Database.Query is

   type A_Measurement_Set is
      record
         num_packages : Natural := 0;
         num_variants : Natural := 0;
         num_ports    : Natural := 0;
         sum_flatsize : Pkgtypes.Package_Size := 0;
         sum_pkgsize  : Pkgtypes.Package_Size := 0;
      end record;

   --  Returns 0 if the package is not registered
   --  Returns package ID if the package is installed
   function package_installed
     (db         : RDB_Connection;
      namebase   : String;
      subpackage : String;
      variant    : String) return Pkgtypes.Package_ID;

   --  Populates a container of installed files given a package ID
   procedure get_package_files
     (db         : RDB_Connection;
      pkg_id     : Pkgtypes.Package_ID;
      files      : in out Archive.Unpack.file_records.Vector);

   --  Returns number of installed packages.  Used for the rvn --status-check
   function number_of_installed_packages
     (db         : RDB_Connection) return Pkgtypes.Package_ID;

   --  Populates a container of truncates packages (only data from main table, no joins)
   procedure rvn_which
     (db         : RDB_Connection;
      query_path : String;
      use_glob   : Boolean;
      packages   : in out Pkgtypes.Package_Set.Vector);

   --  Returns 0 or 1 packages that provide the given library soname
   procedure provides_library
     (db         : RDB_Connection;
      lib_soname : String;
      packages   : in out Pkgtypes.Package_Set.Vector);

   --  Returns 0 or more packages that require the given library soname
   procedure requires_library
     (db         : RDB_Connection;
      lib_soname : String;
      packages   : in out Pkgtypes.Package_Set.Vector);

   --  Returns map of all packages in the remote repository
   --  digest10 => <namebase>-<subpackage>-<variant>-<version>~<digest10>.rvn
   procedure all_remote_packages
     (db         : RDB_Connection;
      file_map   : in out Pkgtypes.NV_Pairs.Map);

   --  Heart of rvn stats(8) command
   procedure package_measurement
     (db           : RDB_Connection;
      measurements : in out A_Measurement_Set);

   procedure finish_package
     (db         : RDB_Connection;
      incomplete : in out Pkgtypes.A_Package);

   procedure finish_package_users
     (db         : RDB_Connection;
      incomplete : in out Pkgtypes.A_Package);

   procedure finish_package_groups
     (db         : RDB_Connection;
      incomplete : in out Pkgtypes.A_Package);

   procedure finish_package_licenses
     (db         : RDB_Connection;
      incomplete : in out Pkgtypes.A_Package);

   procedure finish_package_categories
     (db         : RDB_Connection;
      incomplete : in out Pkgtypes.A_Package);

   procedure finish_package_libs_required
     (db         : RDB_Connection;
      incomplete : in out Pkgtypes.A_Package);

   procedure finish_package_libs_provided
     (db         : RDB_Connection;
      incomplete : in out Pkgtypes.A_Package);

   procedure finish_package_libs_adjacent
     (db         : RDB_Connection;
      incomplete : in out Pkgtypes.A_Package);

   procedure finish_package_directories
     (db         : RDB_Connection;
      incomplete : in out Pkgtypes.A_Package);

   procedure finish_package_dependencies
     (db         : RDB_Connection;
      incomplete : in out Pkgtypes.A_Package);

   procedure finish_package_annotations
     (db         : RDB_Connection;
      incomplete : in out Pkgtypes.A_Package);

   procedure finish_package_options
     (db         : RDB_Connection;
      incomplete : in out Pkgtypes.A_Package);

   procedure finish_package_messages
     (db         : RDB_Connection;
      incomplete : in out Pkgtypes.A_Package);

   procedure finish_package_scripts
     (db         : RDB_Connection;
      incomplete : in out Pkgtypes.A_Package);

   procedure finish_package_files
     (db         : RDB_Connection;
      incomplete : in out Pkgtypes.A_Package);

private

   internal_srcfile : constant String := "raven-database-query.adb";

end Raven.Database.Query;
