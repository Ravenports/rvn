--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Pkgtypes;
with Ada.Containers.Hashed_Maps;

package Raven.Triggers is

   type A_Trigger_Set is tagged private;
   type A_Trigger_Type is (cleanup, installation);

   --  initialize object
   procedure set_rootdir
     (trigger_set  : in out A_Trigger_Set;
      rootdir      : String);

   --  Record triggers
   procedure insert
     (trigger_set     : in out A_Trigger_Set;
      trigger_id      : Natural;
      trigger_type    : A_Trigger_Type;
      script_code     : String;
      entity_path     : String;
      origin_namebase : String;
      origin_subpkg   : String;
      origin_variant  : String;
      origin_prefix   : String;
      upgrading       : boolean);

   --  Execute all triggered scripts
   procedure execute
     (trigger_set  : in out A_Trigger_Set);

   --  Iterates through files to assemble the directory map
   --  Package-owned directories are also added
   procedure gather_directories
     (trigger_set   : in out A_Trigger_Set;
      installed_pkg : Pkgtypes.A_Package;
      upgrading     : Boolean);

   --  Given the dir_path of a trigger definition, returns True if any of the installed
   --  packages created this directory or installed files in that directory.
   function dir_path_matches
     (trigger_set   : A_Trigger_Set;
      dir_path      : String) return Boolean;

   --  Returns true if the directory was touched by an upgraded package
   function dir_path_upgrade
     (trigger_set   : A_Trigger_Set;
      dir_path      : String) return Boolean;

   --  Returns true if the given package is being upgraded (false if ID is unknown internally)
   function package_upgrade
     (trigger_set   : A_Trigger_Set;
      pkg_id        : Pkgtypes.Package_ID) return Boolean;

   --  Returns string "(a,b,c,...)" where a,b,c, etc are the internally stored package IDs.
   --  These are used for SQL conditions
   function id_selection
     (trigger_set   : A_Trigger_Set) return String;

   --  Returns true if cleanup script will be run (meaning directory prune needs to be
   --  performed on rootdir after triggers are run)
   function will_cleanup
      (trigger_set   : A_Trigger_Set) return Boolean;

private

   type TScript is
      record
         trigger_type    : A_Trigger_Type := cleanup;
         script_code     : Text;
         arguments       : Pkgtypes.Text_List.Vector;
         origin_namebase : Text;
         origin_subpkg   : Text;
         origin_variant  : Text;
         origin_prefix   : Text;
         upgrading       : Boolean := False;
      end record;

   function trigger_hash (trigger_id : Natural) return Ada.Containers.Hash_Type;
   function package_id_hash (pkg_id : Pkgtypes.Package_ID) return Ada.Containers.Hash_Type;

   package A_Trigger_Map is new Ada.Containers.Hashed_Maps
     (Key_Type        => Natural,
      Element_Type    => TScript,
      Hash            => trigger_hash,
      Equivalent_Keys => "=");

   package A_Pkg_Upgrade_Set is new Ada.Containers.Hashed_Maps
     (Key_Type        => Pkgtypes.Package_ID,
      Element_Type    => Boolean,
      Hash            => package_id_hash,
      Equivalent_Keys => Pkgtypes."=");

   type A_Trigger_Set is tagged
      record
         trigger_map   : A_Trigger_Map.Map;
         directory_map : Pkgtypes.NV_Pairs.Map;
         relevant_pkgs : A_Pkg_Upgrade_Set.Map;
         rootdir_txt   : Text;
         hit_cleanup   : Boolean := False;
      end record;


end Raven.Triggers;
