--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

private with interfaces.C.Strings;
private with System.Multiprocessors;

package Raven.Cmd.Genrepo is

   --  Executes Genrepo command
   function execute_genrepo_command (comline : Cldata) return Boolean;

private

   package IC renames Interfaces.C;
   package MX renames System.Multiprocessors;

   MAX_SCANNERS  : constant MX.CPU_Range := 16;
   CAT_UCL       : constant String := "catalog.ucl";
   CAT_RVN       : constant String := "catalog.rvn";
   CAT_SUM       : constant String := "catalog.sum";
   CAT_SIGNATURE : constant String := "signature";
   REPO_PUBKEY   : constant String := "repository.pub";

   function analyze_package_files
     (repo_path   : String;
      number_cpus : MX.CPU_Range;
      quiet       : Boolean;
      catalog     : String) return Boolean;

   function create_catalog_digest_file
     (repo_path : String) return Boolean;

   function compress_catalog
     (repo_path          : String;
      provided_pubkey    : Boolean;
      provided_signature : Boolean) return Boolean;

   --  Signs the Blake3 checksum of the catalog
   function create_signature_file
     (repo_path : String;
      key_path  : String;
      catalog   : String) return Boolean;

   --  Verify the signature against the Blake3 checksum of the catalog
   function verify_signed_catalog
     (repo_path : String;
      key_path  : String;
      catalog   : String) return Boolean;

end Raven.Cmd.Genrepo;
