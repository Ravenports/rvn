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
      installed_pkg : Pkgtypes.A_Package);

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
         upgrading       : Boolean;
      end record;

   function trigger_hash (trigger_id : Natural) return Ada.Containers.Hash_Type;

   package A_Trigger_Map is new Ada.Containers.Hashed_Maps
     (Key_Type        => Natural,
      Element_Type    => TScript,
      Hash            => trigger_hash,
      Equivalent_Keys => "=");

   type A_Trigger_Set is tagged
      record
         trigger_map   : A_Trigger_Map.Map;
         directory_map : Pkgtypes.NV_Pairs.Map;
         rootdir_txt   : Text;
      end record;


end Raven.Triggers;
