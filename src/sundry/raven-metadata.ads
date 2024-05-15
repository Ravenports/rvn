--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with ThickUCL;
with Archive.Unpack;
with Raven.Pkgtypes;

private with archive.Whitelist;

package Raven.Metadata is

   type metadata_field is
     (namebase,
      subpackage,
      variant,
      version,
      comment,
      description,
      website,
      maintainer,
      prefix,
      rvndigest,
      rvnsize,
      flatsize,
      abi,
      dependencies,
      options,
      categories,
      license_logic,
      licenses,
      annotations,
      users,
      groups,
      shlibs_provided,
      shlibs_required,
      shlibs_adjacent,
      scripts,
      directories,
      messages
     );

   type message_type is (always, install, deinstall, upgrade);

   --  convert field to key string
   function metadata_field_label (field : metadata_field) return String;

   --  Returns binary power of 2 units.
   --  Integers only, round to nearest (xB, xKiB, xMiB, xGiB)
   function human_readable_size (size : int64) return String;

   --  Return flatsize and rvnsize metadata
   function get_size (metatree : ThickUCL.UclTree; field : metadata_field) return Int64;

   --  Returns the message as function of the phase (install, deinstall, upgrade)
   function get_message
     (metatree : ThickUCL.UclTree;
      phase : message_type) return String;

   --  The dependencies are stored in an object, and this procedure retrieves the sorted keys
   procedure obtain_dependencies_keys
     (metatree : ThickUCL.UclTree;
      dep_keys : in out ThickUCL.jar_string.Vector);

   procedure obtain_annotations_keys
     (metatree  : ThickUCL.UclTree;
      note_keys : in out ThickUCL.jar_string.Vector);

   --  Returns true if key exists and contains string data
   function string_data_exists
     (metatree  : ThickUCL.UclTree;
      field     : metadata_field) return Boolean;

   --  Returns String of metadata field (blank if the field isn't defined)
   function get_string_data
     (metatree  : ThickUCL.UclTree;
      field     : metadata_field) return String;

   --  Returns String of metadata field (default if the field isn't defined)
   function get_string_data
     (metatree  : ThickUCL.UclTree;
      field     : metadata_field;
      default   : String) return String;

   function reveal_namebase   (metatree : ThickUCL.UclTree) return String;
   function reveal_subpackage (metatree : ThickUCL.UclTree) return String;
   function reveal_variant    (metatree : ThickUCL.UclTree) return String;
   function reveal_version    (metatree : ThickUCL.UclTree) return String;
   function reveal_prefix     (metatree : ThickUCL.UclTree) return String;

   procedure convert_to_package
     (metatree  : ThickUCL.UclTree;
      files     : Archive.Unpack.file_records.Vector;
      new_pkg   : in out Pkgtypes.A_Package;
      automatic : Boolean);

private

   package WL renames Archive.Whitelist;

   --  convert message type to key string
   function message_key (mtype : message_type) return String;

   --  extract lists out of metadata.
   procedure set_list
     (metatree : ThickUCL.UclTree;
      field    : metadata_field;
      new_list : in out Pkgtypes.Text_List.Vector);

   Procedure set_nvpair
     (metatree : ThickUCL.UclTree;
      field    : metadata_field;
      new_dict : in out Pkgtypes.NV_Pairs.Map);

   procedure set_scripts
     (metatree : ThickUCL.UclTree;
      new_dict : in out Pkgtypes.Script_Set);

   function convert_to_phase (S : String; phase : in out WL.package_phase) return Boolean;

end Raven.Metadata;
