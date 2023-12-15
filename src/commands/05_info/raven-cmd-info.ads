--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

private with ThickUCL;
private with Raven.Metadata;

package Raven.Cmd.Info is

   --  Executes info command
   function execute_info_command (comline : Cldata) return Boolean;

private

   package MET renames Raven.Metadata;

   not_present_in_metadata : constant String := "package is missing this information";
   wrong_type              : constant String := "error - wrong data type";

   subtype attr_label is String (1 .. 16);

   function format_label (label_text : String) return attr_label;

   procedure display_full_information
     (metatree : ThickUCL.UclTree;
      pkg_path : String;
      quiet    : Boolean);

   procedure display_individual_attributes
     (metatree  : ThickUCL.UclTree;
      comline   : Cldata;
      num_attr  : Natural;
      rvn_path  : String);

   procedure display_string
     (metatree : ThickUCL.UclTree;
      active   : Boolean;
      single   : Boolean;
      mfield   : MET.metadata_field);

   procedure display_size
     (metatree : ThickUCL.UclTree;
      active   : Boolean;
      single   : Boolean);

   procedure display_install_message
     (metatree : ThickUCL.UclTree;
      active   : Boolean;
      single   : Boolean);

   procedure display_array
     (metatree : ThickUCL.UclTree;
      active   : Boolean;
      single   : Boolean;
      quiet    : Boolean;
      mfield   : MET.metadata_field);

   procedure display_array_oneline
     (metatree : ThickUCL.UclTree;
      mfield   : MET.metadata_field);

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

end Raven.Cmd.Info;
