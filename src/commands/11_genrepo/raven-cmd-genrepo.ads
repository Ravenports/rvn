--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

private with System.Multiprocessors;

package Raven.Cmd.Genrepo is

   --  Executes Genrepo command
   function execute_genrepo_command (comline : Cldata) return Boolean;

private

   package MX renames System.Multiprocessors;

   MAX_SCANNERS : constant MX.CPU_Range := 16;

   function analyze_package_files
     (repo_path   : String;
      number_cpus : MX.CPU_Range;
      quiet       : Boolean;
      catalog     : String) return Boolean;

end Raven.Cmd.Genrepo;
