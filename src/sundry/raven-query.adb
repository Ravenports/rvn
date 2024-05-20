--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Strings;  use Raven.Strings;

package body Raven.Query is

   -----------------
   --  get_token  --
   -----------------
   function get_token (component : String) return A_Token
   is
      --  the binary search is case insenstive

      total_keywords : constant Positive := A_Token'Pos (A_Token'Last) + 1;

      subtype keyword_string is String (1 .. 10);

      type keyword_pair is
         record
            keyword : keyword_string;
            keytype : A_Token;
         end record;

      --  Keep in alphabetical order (critical!)
      all_keywords : constant array (1 .. total_keywords) of keyword_pair :=
        (
         ("NOTFOUND  ", token_unrecognized),
         ("#cats     ", token_num_categories),
         ("#deps     ", token_num_dependencies),
         ("#dirs     ", token_num_directories),
         ("#files    ", token_num_files),
         ("#groups   ", token_num_groups),
         ("#lics     ", token_num_licenses),
         ("#notes    ", token_num_annotations),
         ("#opts     ", token_num_options),
         ("#rdeps    ", token_num_reverse_deps),
         ("#shadj    ", token_num_shlibs_adj),
         ("#shpro    ", token_num_shlibs_pro),
         ("#shreq    ", token_num_shlibs_req),
         ("#users    ", token_num_users),
         ("abi       ", token_abi),
         ("auto      ", token_automatic),
         ("comment   ", token_comment),
         ("desc      ", token_description),
         ("logic     ", token_license_logic),
         ("maint     ", token_maintainer),
         ("name      ", token_namebase),
         ("nsv       ", token_nsv),
         ("prefix    ", token_prefix),
         ("size:bytes", token_size_bytes),
         ("size:iec  ", token_size_iec_units),
         ("subpkg    ", token_subpackage),
         ("timestamp ", token_install_time),
         ("variant   ", token_variant),
         ("www       ", token_www_site),
         ("xcat      ", token_ml_categories),
         ("xdep:n    ", token_ml_deps_namebase),
         ("xdep:nsv  ", token_ml_deps_nsv),
         ("xdep:s    ", token_ml_deps_spkg),
         ("xdep:v    ", token_ml_deps_variant),
         ("xdep:ver  ", token_ml_deps_version),
         ("xdir      ", token_ml_directories),
         ("xfile:path", token_ml_files_path),
         ("xfile:sum ", token_ml_files_digest),
         ("xgroup    ", token_ml_groups),
         ("xlic      ", token_ml_licenses),
         ("xnote:key ", token_ml_notes_key),
         ("xnote:val ", token_ml_notes_values),
         ("xopt:def  ", token_ml_opt_default),
         ("xopt:desc ", token_ml_opt_desc),
         ("xopt:key  ", token_ml_opt_key),
         ("xopt:val  ", token_ml_opt_value),
         ("xrdep:n   ", token_ml_rdep_namebase),
         ("xrdep:nsv ", token_ml_rdep_nsv),
         ("xrdep:s   ", token_ml_rdep_spkg),
         ("xrdep:v   ", token_ml_rdep_variant),
         ("xrdep:ver ", token_ml_rdep_version),
         ("xshadj    ", token_ml_shlibs_adj),
         ("xshpro    ", token_ml_shlibs_pro),
         ("xshreq    ", token_ml_shlibs_req),
         ("xuser     ", token_ml_users)
        );

      bandolier : keyword_string := (others => ' ');
      Low       : Natural := all_keywords'First;
      High      : Natural := all_keywords'Last;
      Mid       : Natural;
   begin
      if component'Length > keyword_string'Length or else
        component'Length < 3
      then
         return token_unrecognized;
      end if;

      bandolier (1 .. component'Length) := lowercase (component);

      loop
         Mid := (Low + High) / 2;
         if bandolier = all_keywords (Mid).keyword  then
            return all_keywords (Mid).keytype;
         elsif bandolier < all_keywords (Mid).keyword then
            exit when Low = Mid;
            High := Mid - 1;
         else
            exit when High = Mid;
            Low := Mid + 1;
         end if;
      end loop;
      return token_unrecognized;
   end get_token;

end Raven.Query;
