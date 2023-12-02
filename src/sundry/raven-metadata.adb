--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ucl;
with Raven.Strings; Use Raven.Strings;

package body Raven.Metadata is

   ----------------------------
   --  metadata_field_label  --
   ----------------------------
   function metadata_field_label (field : metadata_field) return String 
   is
   begin
      case field is
         when namebase        => return "namebase";
         when subpackage      => return "subpackage";
         when variant         => return "variant";
         when version         => return "version";
         when comment         => return "comment";
         when description     => return "desc";
         when website         => return "www";
         when maintainer      => return "maintainer";
         when prefix          => return "prefix";
         when flatsize        => return "flatsize";
         when abi             => return "abi";
         when dependencies    => return "deps";
         when options         => return "options";
         when categories      => return "categories"; 
         when license_logic   => return "licenselogic";
         when licenses        => return "licenses";
         when annotations     => return "annotations";
         when users           => return "users";
         when groups          => return "groups";
         when shlibs_provided => return "shlibs_provided";
         when shlibs_required => return "shlibs_required";
         when shlibs_adjacent => return "shlibs_adjacent";
         when scripts         => return "scripts";
         when directories     => return "directories";
         when messages        => return "messages";
      end case;
   end metadata_field_label;

   ---------------------------
   --  human_readable_size  --
   ---------------------------
   function human_readable_size (size : int64) return String 
   is
      --  0             -         5,120 : xB
      --  5,121         -     5,242,880 : 5KiB - 5120KiB
      --  5,242,881     - 5,368,709,120 : 5MiB - 5120MiB
      --  5,368,709,121 -      infinity : 5GiB -
      half_KiB : constant int64 := 512;
      half_MiB : constant int64 := 524_288;
      half_GiB : constant int64 := 536_870_912;
      tmp_size : float;
   begin
      if size < 0 then
         return "----";
      elsif size < 5_121 then
         return int2str (Integer (size)) & " Bytes";
      elsif size < 5_242_881 then
         tmp_size := Float (size + half_KiB) / Float (half_KiB * 2);
         return int2str (Integer (Float'Floor (tmp_size))) & " KiB";
      elsif size < 5_368_709_121 then
         tmp_size := Float (size + half_MiB) / Float (half_MiB * 2);
         return int2str (Integer (Float'Floor (tmp_size))) & " MiB";
      else
         tmp_size := Float (size + half_GiB) / Float (half_GiB * 2);
         return int2str (Integer (Float'Floor (tmp_size))) & " GiB";
      end if;
   end human_readable_size;
   
   
   --------------------
   --  get_flatsize  --
   --------------------
   function get_flatsize (metatree : ThickUCL.UclTree) return Int64 
   is
      dtype      : ThickUCL.Leaf_type;
      key        : constant String := metadata_field_label (flatsize);
      val        : Ucl.ucl_integer;
   begin
      dtype := ThickUCL.get_data_type (metatree, key);
      case dtype is
         when ThickUCL.data_integer =>
            val := metatree.get_base_value (key);
            return Int64 (val);
         when others => return 0;
      end case;
   end get_flatsize;
   
   
   -------------------
   --  get_message  --
   -------------------
   function get_message (metatree : ThickUCL.UclTree; phase : message_type) return String
   is
      dtype : ThickUCL.Leaf_type;
      mtype : ThickUCL.Leaf_type;
      key   : constant String := metadata_field_label (messages);
      mkey  : constant String := message_key (phase);
      vndx  : ThickUCL.object_index;
   begin
      dtype := ThickUCL.get_data_type (metatree, key);
      case dtype is
         when ThickUCL.data_object =>
            vndx := metatree.get_index_of_base_ucl_object (key);
            mtype := metatree.get_object_data_type (vndx, mkey);
            case mtype is
               when ThickUCL.data_string =>
                  return metatree.get_object_value (vndx, mkey);
               when others =>
                  return "";  --  no message of this type (common)
            end case;
         when others =>
            return "Package construction error - message not stored in object";
      end case;
   end get_message;
   
   
   -------------------
   --  message_key  --
   -------------------
   function message_key (mtype : message_type) return String is
   begin
      case mtype is
         when install   => return "install";
         when deinstall => return "deinstall";
         when upgrade   => return "upgrade";
      end case;
   end message_key;
   
   
   --------------------------------
   --  obtain_dependencies_keys  --
   --------------------------------
   procedure obtain_dependencies_keys 
     (metatree : ThickUCL.UclTree; 
      dep_keys : in out ThickUCL.jar_string.Vector) 
   is
      key   : constant String := metadata_field_label (dependencies);
      dtype : ThickUCL.Leaf_type;
      vndx  : ThickUCL.object_index;
   begin
      dtype := ThickUCL.get_data_type (metatree, key);
      case dtype is
         when ThickUCL.data_object =>
            vndx := metatree.get_index_of_base_ucl_object (key);
            metatree.get_object_object_keys (vndx, dep_keys);
         when others => 
            null;
      end case;
   end obtain_dependencies_keys;
   
   
   -------------------------------
   --  obtain_annotations_keys  --
   -------------------------------
   procedure obtain_annotations_keys
     (metatree  : ThickUCL.UclTree; 
      note_keys : in out ThickUCL.jar_string.Vector)
   is
      key   : constant String := metadata_field_label (annotations);
      dtype : ThickUCL.Leaf_type;
      vndx  : ThickUCL.object_index;
   begin
      dtype := ThickUCL.get_data_type (metatree, key);
      case dtype is
         when ThickUCL.data_object =>
            vndx := metatree.get_index_of_base_ucl_object (key);
            metatree.get_object_object_keys (vndx, note_keys);
         when others => 
            null;
      end case;
   end obtain_annotations_keys;
   
   
   --------------------------
   --  string_data_exists  --
   --------------------------
   function string_data_exists 
     (metatree  : ThickUCL.UclTree; 
      field : metadata_field) return Boolean
   is
      dtype : ThickUCL.Leaf_type;
      key   : constant String := metadata_field_label (field);
   begin
      dtype := ThickUCL.get_data_type (metatree, key);
      case dtype is
         when ThickUCL.data_string => return True;
         when others => return False;
      end case;
   end string_data_exists;
   
   
   -----------------------
   --  get_string_data  --
   -----------------------
   function get_string_data 
     (metatree  : ThickUCL.UclTree; 
      field     : metadata_field) return String
   is
      key : constant String := metadata_field_label (field);
   begin
      if string_data_exists (metatree, field) then
         return metatree.get_base_value (key);
      end if;
      return "";
   end get_string_data;
   
   
end Raven.Metadata;
