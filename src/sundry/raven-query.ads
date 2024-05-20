--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

package Raven.Query is

   --  to be private

   type A_Token is
     (token_unrecognized,
      token_num_categories,
      token_num_dependencies,
      token_num_directories,
      token_num_files,
      token_num_groups,
      token_num_licenses,
      token_num_annotations,
      token_num_options,
      token_num_reverse_deps,
      token_num_shlibs_adj,
      token_num_shlibs_pro,
      token_num_shlibs_req,
      token_num_users,
      token_abi,
      token_automatic,
      token_comment,
      token_description,
      token_license_logic,
      token_maintainer,
      token_namebase,
      token_nsv,
      token_prefix,
      token_size_bytes,
      token_size_iec_units,
      token_subpackage,
      token_install_time,
      token_variant,
      token_www_site,
      token_ml_categories,
      token_ml_deps_namebase,
      token_ml_deps_nsv,
      token_ml_deps_spkg,
      token_ml_deps_variant,
      token_ml_deps_version,
      token_ml_directories,
      token_ml_files_path,
      token_ml_files_digest,
      token_ml_groups,
      token_ml_licenses,
      token_ml_notes_key,
      token_ml_notes_values,
      token_ml_opt_default,
      token_ml_opt_desc,
      token_ml_opt_key,
      token_ml_opt_value,
      token_ml_rdep_namebase,
      token_ml_rdep_nsv,
      token_ml_rdep_spkg,
      token_ml_rdep_variant,
      token_ml_rdep_version,
      token_ml_shlibs_adj,
      token_ml_shlibs_pro,
      token_ml_shlibs_req,
      token_ml_users);

   function get_token (component : String) return A_Token;

end Raven.Query;
