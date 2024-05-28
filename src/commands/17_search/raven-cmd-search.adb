--  SPDX-License-Identifier: ISC
--  Reference: /License.txt


with Raven.Event;
with Raven.Context;
with Raven.Pkgtypes;
with Raven.Repository;
with Raven.Database.Search;
with Raven.Database.Operations;
with Raven.Strings; use Raven.Strings;
with Archive.Unix;

package body Raven.Cmd.Search is

   package SEA renames Raven.Database.Search;
   package OPS renames Raven.Database.Operations;

   ------------------------------
   --  execute_search_command  --
   ------------------------------
   function execute_search_command (comline : Cldata) return Boolean
   is
      procedure main_attribute (thismod : Modifier_Data; attribute : String; quiet : Boolean);

      rdb : Database.RDB_Connection;
      behave_cs : Boolean := comline.common_options.case_sensitive;
      packages  : Pkgtypes.Package_Set.Vector;
      magic_col : constant Natural := 39;

      procedure main_attribute (thismod : Modifier_Data; attribute : String; quiet : Boolean)
      is
         nextline : Text;
      begin
         if comline.cmd_search.modifiers (thismod) or else
           thismod = comment
         then
            if not quiet then
               SU.Append (nextline, format_extra (thismod));
            end if;
            SU.Append (nextline, attribute);
            Event.emit_message (USS (nextline));
         end if;
      end main_attribute;

      procedure display_package (position : Pkgtypes.Package_Set.Cursor)
      is
         pkg : Pkgtypes.A_Package renames Pkgtypes.Package_Set.Element (Position);
         quiet : Boolean renames comline.common_options.quiet;
         ident : constant String := Pkgtypes.nsvv_identifier (pkg);
         topline : Text;
      begin
         if quiet or else
           comline.cmd_search.num_modifiers > 0
         then
            Event.emit_message (ident);
         else
            if ident'Length >= magic_col then
               SU.append (topline, ident);
            else
               SU.Append (topline, pad_right (ident, magic_col));
            end if;
            case comline.cmd_search.search is
               when search_unset => null;  --  impossible
               when comment     => SU.Append (topline, pkg.comment);
               when namebase    => SU.Append (topline, pkg.namebase);
               when description => SU.Append (topline, pkg.desc);
               when triplet     => SU.Append (topline, head (ident, "-"));
            end case;
            Event.emit_message (USS (topline));
         end if;
         if comline.cmd_search.num_modifiers = 0 then
            return;
         end if;

         --  At least one modifier set, so automatically include the comment too
         --  (implemented in main attribute)

         --  Query modifier lines
         main_attribute (namebase, USS (pkg.namebase), quiet);
         main_attribute (subpackage, USS (pkg.subpackage), quiet);
         main_attribute (variant, USS (pkg.variant), quiet);
         main_attribute (pkgversion, USS (pkg.version), quiet);
         main_attribute (comment, USS (pkg.comment), quiet);
         if comline.cmd_search.modifiers (categories) then
            SEA.print_categories (rdb, get_extra (categories, quiet), pkg.id);
         end if;
         main_attribute (abi, USS (pkg.abi), quiet);
         if comline.cmd_search.modifiers (licenses) then
            SEA.print_licenses (rdb, get_extra (licenses, quiet), pkg.id, pkg.licenselogic);
         end if;
         main_attribute (www, USS (pkg.www), quiet);
         main_attribute (maintainer, USS (pkg.maintainer), quiet);
         main_attribute (prefix, USS (pkg.prefix), quiet);
         if comline.cmd_search.modifiers (options) then
            SEA.print_options (rdb, get_extra (options, quiet), pkg.id);
         end if;
         if comline.cmd_search.modifiers (shlibs_req) then
            SEA.print_libraries_required (rdb, get_extra (shlibs_req, quiet), pkg.id);
         end if;
         if comline.cmd_search.modifiers (shlibs_prov) then
            SEA.print_libraries_provided (rdb, get_extra (shlibs_prov, quiet), pkg.id);
         end if;
         if comline.cmd_search.modifiers (annotations) then
            SEA.print_annotations (rdb, get_extra (annotations, quiet), pkg.id);
         end if;
         main_attribute (size, trim (pkg.flatsize'Img), quiet);
         main_attribute (rvnsize, trim (pkg.rvnsize'Img), quiet);
         main_attribute (description, Character'Val (10) & USS (pkg.desc), quiet);
         if comline.cmd_search.modifiers (required_by) then
            SEA.print_reverse_dependencies (rdb, get_extra (required_by, quiet), pkg.id);
         end if;
         if comline.cmd_search.modifiers (dependencies) then
            SEA.print_dependencies (rdb, get_extra (dependencies, quiet), pkg.id);
         end if;

      end display_package;

   begin
      if not refresh_catalog (USS (comline.common_options.repo_name)) then
         return False;
      end if;

      case OPS.rdb_open_localdb (rdb, Database.catalog) is
         when RESULT_OK => null;
         when others => return False;
      end case;

      if Context.reveal_case_sensitive then
         behave_cs := True;
      end if;

      SEA.rvn_core_search
        (db           => rdb,
         srch_pattern => USS (comline.cmd_search.spattern),
         behave_glob  => comline.cmd_search.glob_input,
         behave_exact => comline.common_options.exact_match,
         behave_cs    => behave_cs,
         s_comment    => comline.cmd_search.search = comment,
         s_desc       => comline.cmd_search.search = description,
         s_nsv        => comline.cmd_search.search = triplet,
         packages     => packages);

      packages.Iterate (display_package'Access);

      OPS.rdb_close (rdb);
      return True;

   end execute_search_command;


   -----------------------
   --  refresh_catalog  --
   -----------------------
   function refresh_catalog (single_repo : String) return Boolean
   is
      mirrors : Repository.A_Repo_Config_Set;
   begin
      if Archive.Unix.user_is_root then
         Repository.load_repository_configurations (mirrors, single_repo);
         if not Repository.create_local_catalog_database
           (remote_repositories  => mirrors,
            forced               => False,
            quiet                => True)
         then
            Event.emit_error ("Failed to update the local catalog");
         end if;
      end if;

      if not OPS.localdb_exists (Database.catalog) then
         Event.emit_error
           ("Catalog database is missing, should be here: " & OPS.localdb_path (Database.catalog));
         return False;
      end if;

      return True;
   end refresh_catalog;


   --------------------
   --  format_extra  --
   --------------------
   function format_extra (thismod : Modifier_Data) return String
   is
   begin
      case thismod is
         when namebase     => return "name         : ";
         when subpackage   => return "subpackage   : ";
         when variant      => return "variant      : ";
         when pkgversion   => return "version      : ";
         when annotations  => return "annotations  : ";
         when maintainer   => return "maintainer   : ";
         when options      => return "options      : ";
         when size         => return "size         : ";
         when rvnsize      => return "rvnsize      : ";
         when www          => return "www          : ";
         when prefix       => return "prefix       : ";
         when abi          => return "abi          : ";
         when licenses     => return "licenses     : ";
         when required_by  => return "required-by  : ";
         when dependencies => return "depends-on   : ";
         when shlibs_prov  => return "lib-provided : ";
         when shlibs_req   => return "lib-required : ";
         when categories   => return "categories   : ";
         when comment      => return "comment      : ";
         when description  => return "description  : ";
         when full         => return "!dev error!! : ";
      end case;
   end format_extra;


   -----------------
   --  get_extra  --
   -----------------
   function get_extra (thismod : Modifier_Data; quiet : Boolean) return String is
   begin
      if quiet then
         return "";
      end if;
      return format_extra (thismod);
   end get_extra;


end Raven.Cmd.Search;
