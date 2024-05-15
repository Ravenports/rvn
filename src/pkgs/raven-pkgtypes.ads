--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with Raven.Miscellaneous;
with Raven.Strings;
with Archive.Whitelist;
with Blake_3;

package Raven.Pkgtypes is

   package CON renames Ada.Containers;

   type Package_ID         is mod 2**64;
   type Package_Size       is mod 2**64;

   type License_Logic is (LICENSE_DUAL, LICENSE_MULTI, LICENSE_SINGLE, LICENSE_UNLISTED);

   Package_Not_Installed : constant Package_ID := 0;

   package Text_List is new CON.Vectors
     (Element_Type => Text,
      Index_Type   => Natural,
      "="          => SU."=");
   package sorter is new Text_List.Generic_Sorting ("<" => SU."<");

   package NV_Pairs is new CON.Hashed_Maps
     (Key_Type        => Text,
      Element_Type    => Text,
      Hash            => Raven.Miscellaneous.map_hash,
      Equivalent_Keys => Strings.equivalent,
      "="             => SU."=");

   type Script_Parameters is
      record
         code : Text;
         args : Text;
      end record;

   package Script_List is new CON.Vectors
     (Element_Type => Script_Parameters,
      Index_Type   => Natural);

   type Script_Set is array (Archive.Whitelist.package_phase) of Script_List.Vector;

   type File_Item is
      record
         path : Text;
         digest : Blake_3.blake3_hash_hex;
      end record;

   package File_List is new CON.Vectors
     (Element_Type => File_Item,
      Index_Type   => Natural);

   type A_Package is
      record
         id            : Package_ID;
         namebase      : Text;
         subpackage    : Text;
         variant       : Text;
         version       : Text;
         maintainer    : Text;
         www           : Text;
         abi           : Text;
         prefix        : Text;
         comment       : Text;
         desc          : Text;
         rvndigest     : Text;
         licenselogic  : License_Logic;
         automatic     : Boolean;
         rvnsize       : Package_Size;
         flatsize      : Package_Size;
         users         : Text_List.Vector;
         groups        : Text_List.Vector;
         licenses      : Text_List.Vector;
         categories    : Text_List.Vector;
         libs_provided : Text_List.Vector;
         libs_required : Text_List.Vector;
         libs_adjacent : Text_List.Vector;
         directories   : Text_List.Vector;
         dependencies  : NV_Pairs.Map;
         annotations   : NV_Pairs.Map;
         options       : NV_Pairs.Map;
         scripts       : Script_Set;
         files         : File_List.Vector;
      end record;

   package Package_Set is new CON.Vectors
     (Element_Type => A_Package,
      Index_Type   => Natural);

   package ID_Set is new CON.Vectors
     (Element_Type => Package_ID,
      Index_Type   => Natural);

   --  returns name-subpackage-variant-version
   function nsvv_identifier (pkg : A_Package) return String;

end Raven.Pkgtypes;
