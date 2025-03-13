--  SPDX-License-Identifier: ISC
--  Reference: /License.txt


private with ThickUCL;
private with Raven.Pkgtypes;

package Raven.Cmd.Audit is

   --  Executes audit command
   function execute_audit_command (comline : Cldata) return Boolean;

private

   function external_test_input (comline : Cldata; testfile : String) return Boolean;
   function file_contents (filename : String; filesize : Natural) return String;

   function contact_vulnerability_server
     (refresh       : Boolean;
      json_input    : String;
      response_tree : in out ThickUCL.UclTree) return Boolean;

   procedure set_patched_cves (patchset : in out Pkgtypes.Text_List.Vector);

   function display_report
     (comline       : Cldata;
      response_tree : ThickUCL.UclTree;
      patchset      : Pkgtypes.Text_List.Vector) return Boolean;

end Raven.Cmd.Audit;
