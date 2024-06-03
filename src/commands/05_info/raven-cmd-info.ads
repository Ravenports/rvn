--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

private with ThickUCL;
private with Raven.Metadata;

package Raven.Cmd.Info is

   --  Executes info command
   function execute_info_command (comline : Cldata) return Boolean;

private

   package MET renames Raven.Metadata;

   subtype attr_label is String (1 .. 16);

   not_present_in_metadata : constant String := "package is missing this information";
   wrong_type              : constant String := "error - wrong data type";
   no_label                : constant attr_label := (others => ' ');

   function format_label (label_text : String) return attr_label;

   procedure display_full_information
     (metatree : ThickUCL.UclTree;
      pkg_path : String);

   procedure display_individual_attributes
     (metatree  : ThickUCL.UclTree;
      comline   : Cldata;
      num_attr  : Natural;
      rvn_path  : String);

   procedure display_string
     (metatree : ThickUCL.UclTree;
      active   : Boolean;
      single   : Boolean;
      quiet    : Boolean;
      mfield   : MET.metadata_field);

   procedure display_size
     (metatree : ThickUCL.UclTree;
      active   : Boolean;
      single   : Boolean;
      quiet    : Boolean);

   procedure display_message
     (metatree : ThickUCL.UclTree;
      active   : Boolean;
      single   : Boolean;
      quiet    : Boolean;
      mtype    : Pkgtypes.Message_Type);

   procedure display_array
     (metatree : ThickUCL.UclTree;
      active   : Boolean;
      single   : Boolean;
      quiet    : Boolean;
      mfield   : MET.metadata_field);

   procedure display_array_oneline
     (metatree : ThickUCL.UclTree;
      mfield   : MET.metadata_field;
      active   : Boolean;
      single   : Boolean;
      quiet    : Boolean);

   procedure display_dependencies
     (metatree : ThickUCL.UclTree;
      active   : Boolean;
      single   : Boolean;
      quiet    : Boolean);

   procedure display_annotations
     (metatree : ThickUCL.UclTree;
      active   : Boolean;
      single   : Boolean;
      quiet    : Boolean);

   procedure list_files
     (rvn_path  : String;
      active    : Boolean;
      single    : Boolean;
      quiet     : Boolean;
      digest    : Boolean;
      extended  : Boolean);

   procedure display_string
     (line     : String;
      active   : Boolean;
      single   : Boolean;
      quiet    : Boolean;
      mfield   : MET.metadata_field);

   procedure display_size
     (value    : Pkgtypes.Package_Size;
      active   : Boolean;
      single   : Boolean;
      quiet    : Boolean);

   procedure display_array
     (tarray   : Pkgtypes.Text_List.Vector;
      active   : Boolean;
      single   : Boolean;
      quiet    : Boolean;
      mfield   : MET.metadata_field);

   procedure display_array_oneline
     (tarray   : Pkgtypes.Text_List.Vector;
      mfield   : MET.metadata_field;
      active   : Boolean;
      single   : Boolean;
      quiet    : Boolean);

   procedure display_annotations
     (dmap     : Pkgtypes.NV_Pairs.Map;
      active   : Boolean;
      single   : Boolean;
      quiet    : Boolean);

   procedure display_dependencies
     (dmap     : Pkgtypes.NV_Pairs.Map;
      active   : Boolean;
      single   : Boolean;
      quiet    : Boolean);

   procedure display_individual_attributes
     (mpkg     : Pkgtypes.A_Package;
      comline  : Cldata;
      num_attr : Natural);

   procedure display_message
     (mpkg     : Pkgtypes.A_Package;
      active   : Boolean;
      single   : Boolean;
      quiet    : Boolean;
      mtype    : Pkgtypes.Message_Type);

   procedure list_files
     (mpkg     : Pkgtypes.A_Package;
      active   : Boolean;
      single   : Boolean;
      quiet    : Boolean;
      digests  : Boolean);

   procedure display_full_information
     (mpkg     : Pkgtypes.A_Package);

   --  Handle query when --file is not used (the normal mode)
   function execute_installed_info_command
     (comline : Cldata;
      num_attr_selected : Natural) return Boolean;

end Raven.Cmd.Info;
