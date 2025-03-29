--  SPDX-License-Identifier: ISC
--  Reference: /License.txt


private with ThickUCL;
private with Raven.Pkgtypes;
private with Ada.Containers.Vectors;

package Raven.Cmd.Audit is

   --  Executes audit command
   function execute_audit_command (comline : Cldata) return Boolean;

private

   type nvv_rec is record
      namebase : Text;
      variant  : Text;
      version  : Text;
   end record;

   type cve_rec is record
      cve_id         : Text;
      patched        : Boolean;
      base_score     : Integer;
      threat_level   : Text;
      exploitability : Integer;
      impact         : Integer;
      description    : Text;
      published      : Text;
      modified       : Text;
      cvss_version   : Integer;  --  0/20/30/31/40
      cvss_vector    : Text;     --  vector string (decode depends on cvss version)
   end record;

   package set_cve is new Ada.Containers.Vectors
     (Element_Type => cve_rec,
      Index_Type   => Natural);

   package set_nvv is new Ada.Containers.Vectors
     (Element_Type => nvv_rec,
      Index_Type   => Natural);

   type cpe_entry is record
      secure             : Boolean;
      vendor             : Text;
      product            : Text;
      installed_packages : set_nvv.Vector;
      vulnerabilities    : set_cve.Vector;
   end record;

   package set_cpe_entries is new Ada.Containers.Vectors
     (Element_Type => cpe_entry,
      Index_Type   => Natural);

   type vector_breakdown is record
      attack_vector       : Text;
      attack_complexity   : Text;
      privileges_required : Text;
      user_interaction    : Text;
      scope               : Text;
      confidentiality     : Text;
      integrity           : Text;
      availability        : Text;
      authentication_20   : Text;
      attack_reqments_40  : Text;
      vsys_confident_40   : Text;
      ssys_confident_40   : Text;
      vsys_integrity_40   : Text;
      ssys_integrity_40   : Text;
      vsys_avail_40       : Text;
      ssys_avail_40       : Text;
   end record;

   function external_test_input (comline : Cldata; testfile : String) return Boolean;
   function expand_vector_string (cvss_version : Integer; vstring: String) return vector_breakdown;

   function contact_vulnerability_server
     (refresh       : Boolean;
      json_input    : String;
      response_tree : in out ThickUCL.UclTree) return Boolean;

   procedure set_patched_cves (patchset : in out Pkgtypes.Text_List.Vector);

   function assemble_data
     (response_tree : ThickUCL.UclTree;
      patchset      : Pkgtypes.Text_List.Vector;
      cpe_entries   : in out set_cpe_entries.Vector) return Boolean;

   procedure display_report
     (comline       : Cldata;
      cpe_entries   : set_cpe_entries.Vector);

   procedure display_single_record
     (comline       : Cldata;
      cpe_entries   : set_cpe_entries.Vector;
      index         : Natural);

   procedure encode_single_record
     (tree          : in out ThickUCL.UclTree;
      comline       : Cldata;
      cpe_entries   : set_cpe_entries.Vector;
      index         : Natural);

   function make_decimal (score : Integer) return String;
   procedure print_description (description : String);

end Raven.Cmd.Audit;
