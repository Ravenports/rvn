--  SPDX-License-Identifier: ISC
--  Reference: /License.txt


with Ada.Containers.Hashed_Maps;
with Raven.Pkgtypes;
with Raven.Miscellaneous;
with Raven.Strings;
with ThickUCL;
with Blake_3;

package Raven.Repository is

   package CON renames Ada.Containers;

   type Mirror_Type is (not_mirrored, SRV_mirror);
   type Signature_Type is (not_signed, public_key, fingerprinted);
   type Repo_priority is range 0 .. 99;

   type A_Repo_Config is
      record
         master       : Boolean := False;
         enabled      : Boolean := False;
         protocol     : IP_support := no_restriction;
         mirror       : Mirror_Type := not_mirrored;
         verification : Signature_Type := not_signed;
         identifier   : Text;
         url          : Text;
         pubkey_path  : Text;
         fprint_dir   : Text;
         scp_private  : Text;
         scp_public   : Text;
         priority     : Repo_priority := 0;
         environ_set  : Pkgtypes.NV_Pairs.Map;
      end record;

   function "=" (left, right : A_Repo_Config) return Boolean;

   package RepoMap is new CON.Hashed_Maps
     (Key_Type        => Text,
      Element_Type    => A_Repo_Config,
      Hash            => Raven.Miscellaneous.map_hash,
      Equivalent_Keys => Strings.equivalent);

   type A_Repo_Config_Set is
      record
         master_assigned   : Boolean := False;
         master_repository : Text;
         repositories      : RepoMap.Map;
         search_order      : Pkgtypes.Text_List.Vector;
      end record;

   procedure load_repository_configurations
     (remote_repositories : in out A_Repo_Config_Set;
      set_single_master   : String := "");

   procedure convert_repo_configs_to_ucl
     (remote_repositories : A_Repo_Config_Set;
      repo_tree           : in out ThickUCL.UclTree);

   function create_local_catalog_database
     (remote_repositories  : A_Repo_Config_Set;
      forced               : Boolean;
      quiet                : Boolean;
      from_catalog_command : Boolean := False) return Boolean;

   zero_repositories_configured : exception;
   invalid_catalog_digest       : exception;

private

   type download_type is (catalog_digest, catalog_archive);
   type fprint_status is (trusted, revoked);

   --  Populates ABI, OSNAME, ARCH, RELEASE
   function expansion_keys return String;

   --  Given a filename, ingests all the repository configurations listed in the file
   --  Normally this is one, but it can handle multiple definitions
   procedure process_repository_configuration
     (file_path : String;
      set_single_master : String;
      remote_repositories : in out A_Repo_Config_Set);

   --  Establishes the search order vector.
   --  Criteria 1: priority (higher number is higher priority)
   --  Criteria 2: alphabetical sort on identical priorities
   procedure define_search_priority (remote_repositories : in out A_Repo_Config_Set);

   --  Returns True if the attempt to get the calalog.sum file from the master repository
   --  is successful.
   function fetch_master_checksum
     (mirrors : A_Repo_Config_Set;
      forced  : Boolean;
      quiet   : Boolean) return Boolean;

   --  cache location
   function cache_directory return String;

   --  etag download location
   function downloaded_etag_path (downfile : download_type) return String;

   --  download file location
   function downloaded_file_path (downfile : download_type) return String;

   --  Obtain url to master copies of download_type elements
   function master_url
     (downfile : download_type;
      mirrors : A_Repo_Config_Set) return String;

   --  Get internet protocol restrictions from master repository
   function master_protocol (mirrors : A_Repo_Config_Set) return IP_support;

   --  Get SCP private key path from the master repository
   function master_scp_private_key (mirrors : A_Repo_Config_Set) return String;

   --  Get SCP public key path from the master repository
   function master_scp_public_key (mirrors : A_Repo_Config_Set) return String;

   --  Obtain Blake3 digest from cached copy of the master repo's catalog digest
   function read_catalog_digest return Blake_3.blake3_hash_hex;

   --  Returns true if the reference log is in the cache at the end of the routine.
   function obtain_reference_catalog
     (mirrors : A_Repo_Config_Set;
      forced  : Boolean;
      quiet   : Boolean;
      cached  : out Boolean) return Boolean;

   --  Returns true if file with the path relative to the mirror's URL was successfully
   --  fetched.  Etags are not used.  If there are multiple sites, all of them are
   --  checked before False is returned.
   function fetch_from_mirror
     (mirrors        : A_Repo_Config_Set;
      relative_path  : String;
      cache_location : String;
      digest         : Blake_3.blake3_hash_hex;
      quiet          : Boolean;
      site_used      : out Natural) return Boolean;

   --  Returns true if the file at file_path exists and has a blake3 checksum that matches
   --  the given digest
   function file_verified
     (file_path : String;
      digest : Blake_3.blake3_hash_hex) return Boolean;

   --  Extracts contents of catalog.rvn in place.  No success message.
   function extract_catalog_contents return Boolean;

   --  Returns True on unsigned catalogs (signature file not present, repo not expecting it)
   --  Returns True if signature can be verified
   function catalog_is_authentic
     (mirrors    : A_Repo_Config_Set;
      site_index : Natural;
      quiet      : Boolean) return Boolean;

   --  Remove files if they exist
   procedure clean (filename : String);

   --  Remove all files related to the catalog (after failed signature verification)
   procedure remove_catalog_files;

   --  Search the fprints_path/trusted directory for ucl-formatted fingerprint files
   --  with blake3 format and fingerprints matching the key_file_digests.  If found, return True
   function confirm_matching_fingerprints
     (key_file_digest : Blake_3.blake3_hash_hex;
      fprints_path    : String;
      status          : fprint_status) return Boolean;

   --  Verify the signature against the Blake3 checksum of the catalog
   function verify_signed_catalog
     (signature_file : String;
      key_path       : String;
      catalog        : String;
      quiet          : Boolean) return Boolean;

   --  Compares url text before "://" to the array of VALID_URL_SCHEME
   --  If it matches one of the elements, True is returned.
   function valid_scheme_specified (url : String) return Boolean;

end Raven.Repository;
