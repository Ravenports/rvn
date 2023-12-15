--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Archive;

package Raven.Cmd.Create is
   
   --  execute_create_command  
   function execute_create_command (comline : Cldata) return Boolean;
   
private
   
   function provide_timestamp (arg : text) return Archive.filetime;
   function valid_directory (checkdir : String; description : String) return Boolean;
   function valid_file (checkfile : String; description : String) return Boolean;
   function rvn_file (filename : String; creation_dir : String) return String;
   function creation_directory (outdir : String) return String;
   
end Raven.Cmd.Create;
