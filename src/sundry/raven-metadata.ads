--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt


with ThickUCL;

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

   type message_type is (install, deinstall, upgrade);
   
   --  convert field to key string
   function metadata_field_label (field : metadata_field) return String;
     
   --  Returns binary power of 2 units.
   --  Integers only, round to nearest (xB, xKiB, xMiB, xGiB)
   function human_readable_size (size : int64) return String;
   
   --  Return metadata flatsize
   function get_flatsize (metatree : ThickUCL.UclTree) return Int64;
   
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
   
private
   
   --  convert message type to key string
   function message_key (mtype : message_type) return String; 
   
end Raven.Metadata;