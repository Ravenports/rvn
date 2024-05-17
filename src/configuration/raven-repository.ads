--  SPDX-License-Identifier: ISC
--  Reference: /License.txt


with Ada.Containers.Hashed_Maps;
with Raven.Pkgtypes;
with Raven.Miscellaneous;
with Raven.Strings;

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

   procedure load_repository_configurations (remote_repositories : in out A_Repo_Config_Set);

private

   --  Populates ABI, OSNAME, ARCH, RELEASE
   function expansion_keys return String;

   --  Given a filename, ingests all the repository configurations listed in the file
   --  Normally this is one, but it can handle multiple definitions
   procedure process_repository_configuration
     (file_path : String;
      remote_repositories : in out A_Repo_Config_Set);

end Raven.Repository;
