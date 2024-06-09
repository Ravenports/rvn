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
   package ARW renames Archive.Whitelist;

   type Package_ID         is mod 2**64;
   type Package_Size       is mod 2**64;
   type Epoch_Timestamp    is mod 2**64;

   type License_Logic is (LICENSE_DUAL, LICENSE_MULTI, LICENSE_SINGLE, LICENSE_UNLISTED);
   type Message_Type is (install, deinstall, upgrade);

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

   type Script_Set is array (ARW.package_phase) of Script_List.Vector;

   type Message_Parameters is
      record
         message : Text;
         minimum_version : Text;
         maximum_version : Text;
      end record;

   package Message_List is new CON.Vectors
     (Element_Type => Message_Parameters,
      Index_Type   => Natural);

   type Message_Set is array (Message_Type) of Message_List.Vector;

   type File_Item is
      record
         path : Text;
         digest : Blake_3.blake3_hash_hex;
      end record;

   package File_List is new CON.Vectors
     (Element_Type => File_Item,
      Index_Type   => Natural);

   type Note_Item is
      record
         tag    : Text;
         note   : Text;
         custom : Boolean;
      end record;

   package NoteSet is new CON.Hashed_Maps
     (Key_Type        => Text,
      Element_Type    => Note_Item,
      Hash            => Raven.Miscellaneous.map_hash,
      Equivalent_Keys => Strings.equivalent);

   type A_Package is
      record
         id            : Package_ID := 0;
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
         licenselogic  : License_Logic := License_Logic'First;
         automatic     : Boolean := False;
         rvnsize       : Package_Size := 0;
         flatsize      : Package_Size := 0;
         install_time  : Epoch_Timestamp := 0;
         users         : Text_List.Vector;
         groups        : Text_List.Vector;
         licenses      : Text_List.Vector;
         categories    : Text_List.Vector;
         libs_provided : Text_List.Vector;
         libs_required : Text_List.Vector;
         libs_adjacent : Text_List.Vector;
         directories   : Text_List.Vector;
         dependencies  : NV_Pairs.Map;
         options       : NV_Pairs.Map;
         annotations   : NoteSet.Map;
         messages      : Message_Set;
         scripts       : Script_Set;
         files         : File_List.Vector;
      end record;

   package Package_Set is new CON.Vectors
     (Element_Type => A_Package,
      Index_Type   => Natural);

   package Package_Map is new CON.Hashed_Maps
     (Key_Type        => Text,
      Element_Type    => A_Package,
      Hash            => Raven.Miscellaneous.map_hash,
      Equivalent_Keys => Strings.equivalent);

   package ID_Set is new CON.Vectors
     (Element_Type => Package_ID,
      Index_Type   => Natural);

   --  returns name-subpackage-variant-version
   function nsvv_identifier (pkg : A_Package) return String;

   --  returns name-subpackage-variant
   function nsv_identifier (pkg : A_Package) return String;

   --  returns messages of the same type (install, remove, upgrade) joined by a line feed
   function combined_messages (pkg : A_Package; mtype : Message_Type) return String;

   --  Returns package size of path, and 0 if path does not exist
   function get_file_size (path : String) return Package_Size;

end Raven.Pkgtypes;
