--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

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
         when always    => return "always";
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


   ---------------------------
   --  get_string_data # 1  --
   ---------------------------
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


   ---------------------------
   --  get_string_data # 2  --
   ---------------------------
   function get_string_data
     (metatree  : ThickUCL.UclTree;
      field     : metadata_field;
      default   : String) return String
   is
      key : constant String := metadata_field_label (field);
   begin
      if string_data_exists (metatree, field) then
         return metatree.get_base_value (key);
      end if;
      return default;
   end get_string_data;


   -----------------------
   --  reveal_namebase  --
   -----------------------
   function reveal_namebase (metatree : ThickUCL.UclTree) return String is
   begin
      return get_string_data (metatree, namebase, "namebase_not_found");
   end reveal_namebase;

   -------------------------
   --  reveal_subpackage  --
   -------------------------
   function reveal_subpackage (metatree : ThickUCL.UclTree) return String is
   begin
      return get_string_data (metatree, subpackage, "subpackage_not_found");
   end reveal_subpackage;

   ---------------------
   --  reveal_variant --
   ---------------------
   function reveal_variant (metatree : ThickUCL.UclTree) return String is
   begin
      return get_string_data (metatree, variant, "variant_not_found");
   end reveal_variant;

   ----------------------
   --  reveal_version  --
   ----------------------
   function reveal_version (metatree : ThickUCL.UclTree) return String is
   begin
      return get_string_data (metatree, version, "version_not_found");
   end reveal_version;

   ---------------------
   --  reveal_prefix  --
   ---------------------
   function reveal_prefix (metatree : ThickUCL.UclTree) return String is
   begin
      return get_string_data (metatree, prefix, "prefix_not_found");
   end reveal_prefix;


   ----------------
   --  set_list  --
   ----------------
   procedure set_list
     (metatree : ThickUCL.UclTree;
      field    : metadata_field;
      new_list : in out Pkgtypes.Text_List.Vector)
   is
      key   : constant String := metadata_field_label (field);
      dtype : ThickUCL.Leaf_type;
      vndx  : ThickUCL.array_index;
      num_elements : Natural;
   begin
      new_list.clear;
      dtype := ThickUCL.get_data_type (metatree, key);
      case dtype is
         when ThickUCL.data_array =>
            vndx := metatree.get_index_of_base_array (key);
            num_elements := metatree.get_number_of_array_elements (vndx);
            for index in 0 .. num_elements - 1 loop
               case metatree.get_array_element_type (vndx, index) is
                  when ThickUCL.data_string =>
                     declare
                        data : constant String := metatree.get_array_element_value (vndx, index);
                     begin
                        new_list.append (SUS (data));
                     end;
                  when others => null;  --  something is wrong if we get here.
               end case;
            end loop;
         when others => null;
      end case;
   end set_list;


   ------------------
   --  set_nvpair  --
   ------------------
   Procedure set_nvpair
     (metatree : ThickUCL.UclTree;
      field    : metadata_field;
      new_dict : in out Pkgtypes.NV_Pairs.Map)
   is
      key   : constant String := metadata_field_label (field);
      dtype : ThickUCL.Leaf_type;
      vndx  : ThickUCL.object_index;
      jar   : ThickUCL.jar_string.Vector;

      procedure insert_nv (Position : ThickUCL.jar_string.Cursor)
      is
         key2 : constant String := USS (ThickUCL.jar_string.Element (Position).payload);
      begin
         case metatree.get_object_data_type (vndx, key2) is
            when ThickUCL.data_string =>
               declare
                  value : constant String := metatree.get_object_value (vndx, key2);
               begin
                  new_dict.Insert (SUS (key2), SUS (value));
               end;
            when others => null;  --problem
         end case;
      end insert_nv;
   begin
      new_dict.Clear;
      dtype := ThickUCL.get_data_type (metatree, key);
      case dtype is
         when ThickUCL.data_object =>
            vndx := metatree.get_index_of_base_ucl_object (key);
            metatree.get_object_object_keys (vndx, jar);
         when others => null;
      end case;
      jar.Iterate (insert_nv'Access);
   end set_nvpair;


   ------------------------
   --  convert_to_phase  --
   ------------------------
   function convert_to_phase (S : String; phase : in out WL.package_phase) return Boolean
   is
   begin
      phase := WL.pre_install;
      if S'Length < 11 then
         return False;
      end if;

      if S (S'Last - 3 .. S'Last) = "-lua" then
         if S = "pre-install-lua" then
            phase := WL.pre_install_lua;
         elsif S = "pre-deinstall-lua" then
            phase := WL.pre_deinstall_lua;
         elsif S = "post-install-lua" then
            phase := WL.post_install_lua;
         elsif S = "post-deinstall-lua" then
            phase := WL.post_deinstall_lua;
         else
            return False;
         end if;
      else
         if S = "pre-install" then
            phase := WL.pre_install;
         elsif S = "pre-deinstall" then
           phase := WL.pre_deinstall;
         elsif S = "post-install" then
            phase := WL.post_install;
         elsif S = "post-deinstall" then
            phase := WL.post_deinstall;
         else
            return False;
         end if;
      end if;

      return True;
   end convert_to_phase;

   -------------------
   --  set_scripts  --
   -------------------
   procedure set_scripts
     (metatree : ThickUCL.UclTree;
      new_dict : in out Pkgtypes.Script_Set)
   is
      key   : constant String := metadata_field_label (scripts);
      vndx  : ThickUCL.object_index;
      jar   : ThickUCL.jar_string.Vector;

      procedure scan_phase (Position : ThickUCL.jar_string.Cursor)
      is
         phase_str   : constant String := USS (ThickUCL.jar_string.Element (Position).payload);
         phase       : WL.package_phase := WL.pre_install;
         num_scripts : Natural;
         andx        : ThickUCL.array_index;
         vndx2       : ThickUCL.object_index;
      begin
         if not convert_to_phase (phase_str, phase) then
            --  failed to convert to a known phase; ignore completely
            return;
         end if;
         andx := metatree.get_object_array (vndx, phase_str);
         num_scripts := metatree.get_number_of_array_elements (andx);
         for index in 0 .. num_scripts - 1 loop
            case metatree.get_array_element_type (andx, index) is
               when ThickUCL.data_object =>
                  vndx2 := metatree.get_array_element_object (andx, index);
                  case metatree.get_object_data_type (vndx2, "args") is
                     when ThickUCL.data_string =>
                        case metatree.get_object_data_type (vndx2, "code") is
                           when ThickUCL.data_string =>
                              declare
                                 sp : Pkgtypes.Script_Parameters;
                                 args : constant String :=
                                   metatree.get_object_value (vndx2, "args");
                                 code : constant String :=
                                   metatree.get_object_value (vndx2, "code");
                              begin
                                 sp.args := SUS (args);
                                 sp.code := SUS (code);
                                 new_dict (phase).Append (sp);
                              end;
                           when others => null;
                        end case;
                     when others => null;
                  end case;
               when others => null;
            end case;
         end loop;
      end scan_phase;
   begin
      case metatree.get_data_type (key) is
         when ThickUCL.data_object => null;
         when others =>
            --  scripts is not an object; corrupt
            return;
      end case;

      vndx := metatree.get_index_of_base_ucl_object (key);
      metatree.get_object_object_keys (vndx, jar);
      jar.Iterate (scan_phase'Access);
   end set_scripts;


   --------------------------
   --  convert_to_package  --
   --------------------------
   procedure convert_to_package
     (metatree  : ThickUCL.UclTree;
      files     : Archive.Unpack.file_records.Vector;
      new_pkg   : in out Pkgtypes.A_Package;
      automatic : Boolean)
   is
      procedure scan (Position : Archive.Unpack.file_records.Cursor)
      is
         FR : Archive.Unpack.file_record renames Archive.Unpack.file_records.Element (Position);
         tray : Pkgtypes.File_Item;
      begin
         tray.digest := FR.digest;
         tray.path   := FR.path;
         new_pkg.files.Append (tray);
      end scan;
   begin
      new_pkg.namebase   := SUS (reveal_namebase (metatree));
      new_pkg.subpackage := SUS (reveal_subpackage (metatree));
      new_pkg.variant    := SUS (reveal_variant (metatree));
      new_pkg.version    := SUS (reveal_version (metatree));
      new_pkg.prefix     := SUS (reveal_prefix (metatree));
      new_pkg.maintainer := SUS (get_string_data (metatree, maintainer, "<nobody>"));
      new_pkg.www        := SUS (get_string_data (metatree, website, ""));
      new_pkg.abi        := SUS (get_string_data (metatree, abi, "*:*:0"));
      new_pkg.comment    := SUS (get_string_data (metatree, comment, "<no summary>"));
      new_pkg.desc       := SUS (get_string_data (metatree, description, "<no description>"));
      new_pkg.flatsize   := Pkgtypes.Package_Size (get_flatsize (metatree));
      new_pkg.automatic  := automatic;

      set_list (metatree, users, new_pkg.users);
      set_list (metatree, groups, new_pkg.groups);
      set_list (metatree, licenses, new_pkg.licenses);
      set_list (metatree, categories, new_pkg.categories);
      set_list (metatree, directories, new_pkg.directories);
      set_list (metatree, shlibs_adjacent, new_pkg.libs_adjacent);
      set_list (metatree, shlibs_provided, new_pkg.libs_provided);
      set_list (metatree, shlibs_required, new_pkg.libs_required);

      new_pkg.files.Clear;
      files.Iterate (scan'Access);

      set_nvpair (metatree, dependencies, new_pkg.dependencies);
      set_nvpair (metatree, annotations, new_pkg.annotations);
      set_nvpair (metatree, options, new_pkg.options);
      set_scripts (metatree, new_pkg.scripts);

      declare
         given_license : constant String := get_string_data (metatree, license_logic, "none");
      begin
         if given_license = "single" then
            new_pkg.licenselogic := Pkgtypes.LICENSE_SINGLE;
         elsif given_license = "dual" then
            new_pkg.licenselogic := Pkgtypes.LICENSE_DUAL;
         elsif given_license = "multi" then
            new_pkg.licenselogic := Pkgtypes.LICENSE_MULTI;
         else
            new_pkg.licenselogic := PkgTypes.LICENSE_UNLISTED;
         end if;
      end;

   end convert_to_package;

end Raven.Metadata;
