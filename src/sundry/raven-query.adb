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
         ("xnote:val ", token_ml_notes_value),
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

   ------------------
   --  get_column  --
   ------------------
   function get_column (token : A_Token) return String
   is
      id  : constant String := "package_id";
      nsv : constant String := "namebase || '-' || subpackage || '-' || variant as nsv";

      function count_subquery (table_name, id_name : String) return String is
      begin
         return "(select count(" & id_name & ") from " & table_name &
           " where " & table_name & "." & id_name & " = id) as " & token'Img;
      end count_subquery;
   begin
      case token is
         when token_unrecognized => raise token_invalid_for_column;

         when token_num_categories   => return count_subquery ("pkg_categories", id);
         when token_num_dependencies => return count_subquery ("pkg_dependencies", id);
         when token_num_directories  => return count_subquery ("pkg_directories", id);
         when token_num_files        => return count_subquery ("pkg_files", id);
         when token_num_groups       => return count_subquery ("pkg_groups", id);
         when token_num_licenses     => return count_subquery ("pkg_licenses", id);
         when token_num_annotations  => return count_subquery ("pkg_annotations", id);
         when token_num_options      => return count_subquery ("pkg_options", id);
         when token_num_reverse_deps => return count_subquery ("pkg_dependencies", "dependency_id");
         when token_num_shlibs_adj   => return count_subquery ("pkg_libs_adjacent", id);
         when token_num_shlibs_pro   => return count_subquery ("pkg_libs_required", id);
         when token_num_shlibs_req   => return count_subquery ("pkg_libs_provided", id);
         when token_num_users        => return count_subquery ("pkg_users", id);
         when token_abi              => return "abi";
         when token_automatic        => return "automatic";
         when token_comment          => return "comment";
         when token_description      => return "desc";
         when token_license_logic    => return "licenselogic";
         when token_maintainer       => return "maintainer";
         when token_namebase         => return "namebase";
         when token_nsv              => return nsv;
         when token_prefix           => return "prefix";
         when token_size_iec_units   |  --  post-process
              token_size_bytes       => return "flatsize";
         when token_subpackage       => return "subpackage";
         when token_install_time     => return "installed";
         when token_variant          => return "variant";
         when token_www_site         => return "www";
            --  The remaining enumerations require joins (limited to one per unique table)
         when token_ml_categories    => return "categories.name AS category";
         when token_ml_deps_namebase |  --  post-process
              token_ml_deps_variant  |  --  post-process
              token_ml_deps_spkg     |  --  post-process
              token_ml_deps_nsv      => return "dependencies.nsv as dep_nsv";
         when token_ml_deps_version  => return "dependencies.version as dep_version";
         when token_ml_directories   => return "directories.path as dir_path";
         when token_ml_files_path    => return "pkg_files.path as file_path";
         when token_ml_files_digest  => return "pkg_files.b3digest as file_sum";
         when token_ml_groups        => return "groups.name as grp_name";
         when token_ml_licenses      => return "licenses.name as lic_name";
         when token_ml_notes_key     => return "annotations.note_key as ann_key";
         when token_ml_notes_value   => return "pkg_annotations.annotation as ann_value";
         when token_ml_opt_key       => return "options.option_name as opt_key";
         when token_ml_opt_value     => return "pkg_options.option_setting as opt_val";
         when token_ml_rdep_namebase |  --  post-process
              token_ml_rdep_spkg     |  --  post-process
              token_ml_rdep_variant  |  --  post-process
              token_ml_rdep_nsv      => return "dependencies.nsv as rdep_nsv";
         when token_ml_rdep_version  => return "dependencies.version as rdep_version";
         when token_ml_shlibs_adj    => return "libraries.name as adj_lib";
         when token_ml_shlibs_pro    => return "libraries.name as pro_lib";
         when token_ml_shlibs_req    => return "libraries.name as pro_req";
         when token_ml_users         => return "users.name as user_name";
      end case;
   end get_column;

end Raven.Query;
