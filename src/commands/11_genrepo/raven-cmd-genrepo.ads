--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

private with Interfaces.C.Strings;
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

   --  Execute the given command.  Returns true when the response is valid, resulting in a
   --  signature and a public key
   function remotely_sign_catalog
     (repo_path : String;
      catalog   : String;
      scommand  : String) return Boolean;

   --  If file size < 4097 chars, read entire file at once.
   --  Otherwise it's not valid anyway and return "INVALID"
   function slurp_sign_output
     (path      : String) return String;

   --  Write directly (avoids unwanted carriage returns)
   procedure generate_file
     (path      : String;
      contents  : String);

   --  Replace all "{}" with digest.
   --  If no "{}" found, add it to the end with a preceeding space
   function construct_remote_command
     (template  : String;
      digest    : String) return String;

end Raven.Cmd.Genrepo;
