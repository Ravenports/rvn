--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Exceptions;
with Ada.Directories;
with Raven.Cmd.Unset;
with Raven.Strings;
with Raven.Context;
with Raven.Catalog;
with Raven.Event;
with Raven.Fetch;
with Raven.Unix;
with Archive.Unix;
with Archive.Misc;
with Archive.Unpack;
with Archive.Dirent.Scan;
with ThickUCL.Files;
with Ucl;
with curl_callbacks;
with Interfaces.C.Strings;

use Raven.Strings;

package body Raven.Repository is

   package RCU renames Raven.Cmd.Unset;
   package DSC renames Archive.Dirent.Scan;
   package CAL renames curl_callbacks;
   package IC  renames Interfaces.C;


   ---------------------------------
   --  A_Repo_Config."=" operator --
   ---------------------------------
   function "=" (left, right : A_Repo_Config) return Boolean is
   begin
      return SU."=" (left.identifier, right.identifier);
   end "=";


   ----------------------------------------
   --  process_repository_configuration  --
   ----------------------------------------
   procedure process_repository_configuration
     (file_path : String;
      set_single_master : String;
      remote_repositories : in out A_Repo_Config_Set)
   is
      conf_tree   : ThickUCL.UclTree;
      identifiers : ThickUCL.jar_string.Vector;
      expkeys     : constant String := expansion_keys;
      override    : constant Boolean := set_single_master /= "";

      procedure process_object (identifier_pos : ThickUCL.jar_string.Cursor)
      is
         identifier : constant String := USS (ThickUCL.jar_string.Element (identifier_pos).payload);
         keys       : ThickUCL.jar_string.Vector;
         cfg_object : ThickUCL.object_index;
         rconfig    : A_Repo_Config;

         procedure process_key (key_pos : ThickUCL.jar_string.Cursor)
         is
            field_key : constant String := USS (ThickUCL.jar_string.Element (key_pos).payload);
            upper_key : constant String := uppercase (field_key);
            unhandled : Boolean := False;
         begin
            case ThickUCL.get_object_data_type (conf_tree, cfg_object, field_key) is
               when ThickUCL.data_boolean =>
                  declare
                     value : constant Boolean := conf_tree.get_object_value (cfg_object, field_key);
                  begin
                     if upper_key = "ENABLED" then
                        rconfig.enabled := value;
                     elsif upper_key = "MASTER" then
                        rconfig.master := value;
                     else
                        unhandled := True;
                     end if;
                  end;
               when ThickUCL.data_integer =>
                  declare
                     value : constant Ucl.ucl_integer :=
                       conf_tree.get_object_value (cfg_object, field_key);
                  begin
                     if upper_key = "IP_VERSION" then
                        case value is
                           when 4 => rconfig.protocol := IPv4_only;
                           when 6 => rconfig.protocol := IPv6_only;
                           when others => rconfig.protocol := no_restriction;
                        end case;
                     elsif upper_key = "PRIORITY" then
                        case value is
                           when 0 .. 99 => rconfig.priority := Repo_priority (value);
                           when others => rconfig.priority := 0;
                        end case;
                     else
                        unhandled := True;
                     end if;
                  end;
               when ThickUCL.data_string =>
                  declare
                     value : constant String := conf_tree.get_object_value (cfg_object, field_key);
                     upper_value : constant String := uppercase (value);
                  begin
                     if upper_key = "URL" then
                        rconfig.url := SUS (value);
                     elsif upper_key = "PUBKEY" then
                        rconfig.pubkey_path := SUS (value);
                     elsif upper_key = "FINGERPRINTS" then
                        rconfig.fprint_dir := SUS (value);
                     elsif upper_key = "MIRROR_TYPE" then
                        if upper_value = "SRV" then
                           rconfig.mirror := SRV_mirror;
                        end if;
                     elsif upper_key = "SIGNATURE_TYPE" then
                        if upper_value = "PUBKEY" then
                           rconfig.verification := public_key;
                        elsif upper_value = "FINGERPRINTS" then
                           rconfig.verification := fingerprinted;
                        end if;
                     else
                        unhandled := True;
                     end if;
                  end;
               when ThickUCL.data_object =>
                  declare
                     objkeys : ThickUCL.jar_string.Vector;
                     vndx    : ThickUCL.object_index;
                     nkeys   : Natural;
                     keytxt  : Text;
                  begin
                     vndx := ThickUCL.get_object_object (conf_tree, cfg_object, field_key);
                     ThickUCL.get_object_object_keys (conf_tree, vndx, objkeys);
                     nkeys := Natural (objkeys.Length);
                     if upper_key = "ENV" then
                        for x in 0 .. nkeys - 1 loop
                           keytxt := objkeys.Element (x).payload;
                           case ThickUCL.get_object_data_type (conf_tree, vndx, USS (keytxt)) is
                              when ThickUCL.data_string =>
                                 declare
                                    val : constant String :=
                                      ThickUCL.get_object_value (conf_tree, vndx, USS (keytxt));
                                 begin
                                    if rconfig.environ_set.Contains (keytxt) then
                                       Event.emit_message
                                         (identifier & " repository env '" & USS (keytxt) &
                                            "' redefinition ignored.");
                                    else
                                       rconfig.environ_set.Insert (keytxt, SUS (val));
                                    end if;
                                 end;
                              when others =>
                                 Event.emit_message
                                   (identifier & " repository env '" & USS (keytxt) &
                                      "' skipped because its value is not a string");
                           end case;
                        end loop;
                     else
                        unhandled := True;
                     end if;
                  end;
               when ThickUCL.data_array =>
                  unhandled := True;
               when ThickUCL.data_float =>
                  unhandled := True;
               when ThickUCL.data_time =>
                  unhandled := True;
               when ThickUCL.data_not_present =>
                  null;
            end case;
            if unhandled then
               Event.emit_message (identifier & " key '" & field_key & "' unrecognized.");
            end if;
         end process_key;

      begin
         if not conf_tree.ucl_object_field_exists (identifier) then
            Event.emit_message
              ("Repo config: Skipped " & identifier & " (does not map to an object)");
         end if;
         cfg_object := conf_tree.get_index_of_base_ucl_object (identifier);
         conf_tree.get_object_object_keys (cfg_object, keys);
         rconfig.identifier := SUS (identifier);
         keys.Iterate (process_key'Access);

         if override then
            if identifier = set_single_master then
               rconfig.master := True;
               rconfig.enabled := True;
               remote_repositories.master_assigned := True;
               remote_repositories.master_repository := SUS (identifier);
               remote_repositories.repositories.Insert (SUS (identifier), rconfig);
            else
               Event.emit_debug (high_level, "ignore " & identifier & " repo due to override");
            end if;
         else
            if rconfig.enabled then
               if rconfig.master then
                  if remote_repositories.master_assigned then
                     Event.emit_message
                       ("Master designation on " & identifier & " repository ignored; " &
                          USS (remote_repositories.master_repository) & " is already master.");
                     rconfig.master := False;
                  else
                     remote_repositories.master_assigned := True;
                     remote_repositories.master_repository := SUS (identifier);
                  end if;
               end if;
               remote_repositories.repositories.Insert (SUS (identifier), rconfig);
            end if;
         end if;
      end process_object;

   begin
      ThickUCL.Files.parse_ucl_file (conf_tree, file_path, expkeys);
      conf_tree.get_base_object_keys (identifiers);
      identifiers.Iterate (process_object'Access);
   exception
      when ThickUCL.Files.ucl_file_unparseable =>
         Event.emit_message ("Repo config: skipped unparsable file " & file_path);
   end process_repository_configuration;


   --------------------------------------
   --  load_repository_configurations  --
   --------------------------------------
   procedure load_repository_configurations (remote_repositories : in out A_Repo_Config_Set;
                                             set_single_master   : String := "")
   is
      delim    : constant Character := Character'Val (0);
      delim2   : constant String (1 .. 1) := (others => delim);
      setting  : constant String := RCU.config_setting_as_string (RCU.CFG.repos_dir);
      numlines : constant Natural := count_char (setting, delim) + 1;

      procedure ingest_repository_config_file (Position : DSC.dscan_crate.Cursor)
      is
         file_path : constant String := DSC.dscan_crate.Element (Position).full_path;
         features  : Archive.Unix.File_Characteristics;
      begin
         --  ignore filenames that don't end in .conf extension, and ensure it is a regular file
         if trails (file_path, ".conf") then
            features := Archive.Unix.get_charactistics (file_path);
            case features.ftype is
               when Archive.regular => null;
               when others => return;
            end case;
            process_repository_configuration (file_path, set_single_master, remote_repositories);
         end if;
      end ingest_repository_config_file;

      procedure set_master (key : Text; Element : in out A_Repo_Config) is
      begin
         Element.master := True;
      end set_master;

   begin
      for line in 1 .. numlines loop
         declare
            dir_repo_config : constant String := specific_field (setting, line, delim2);
            files : DSC.dscan_crate.Vector;
         begin
            DSC.scan_directory (dir_repo_config, files);
            files.Iterate (ingest_repository_config_file'Access);
         exception
            when DSC.dscan_open_failure => null;  --  directory probably doesn't exist
            when unexpected : DSC.dscan_folder_already_open |
                 DSC.dscan_folder_not_open |
                 DSC.dscan_close_failure =>  --  unexpected error, add to debug messages
               Event.emit_debug (high_level, Ada.Exceptions.Exception_Message (unexpected));
         end;
      end loop;
      if not remote_repositories.repositories.Is_Empty then
         define_search_priority (remote_repositories);
      end if;

      if not remote_repositories.master_assigned then
         --  None of the enable repositories are remote, so designate the
         --  highest priority repository to be master
         if not remote_repositories.search_order.Is_Empty then
            remote_repositories.master_repository := remote_repositories.search_order.First_Element;
            declare
               cursor : constant RepoMap.Cursor :=
                 remote_repositories.repositories.Find (remote_repositories.master_repository);
            begin
               remote_repositories.repositories.Update_Element (cursor, set_master'Access);
            end;
            remote_repositories.master_assigned := True;
         end if;
      end if;
   end load_repository_configurations;


   ----------------------
   --  expansion_keys  --
   ----------------------
   function expansion_keys return String
   is
      abi     : constant String := Archive.Misc.determine_abi;
      osname  : constant String := specific_field (abi, 1, ":");
      arch    : constant String := specific_field (abi, 2, ":");
      release : constant String := specific_field (abi, 3, ":");
   begin
      return "ABI=" & abi & "|OSNAME=" & osname & "|ARCH=" & arch & "|RELEASE=" & release;
   end expansion_keys;


   ------------------------------
   --  define_search_priority  --
   ------------------------------
   procedure define_search_priority (remote_repositories : in out A_Repo_Config_Set)
   is
      temp_list : Pkgtypes.Text_List.Vector;
      sep       : constant Character := Character'Val (9);

      procedure populate_temp_list (Position : RepoMap.Cursor)
      is
         identifier : Text renames RepoMap.Key (Position);
         priority   : Repo_priority renames RepoMap.Element (Position).priority;
      begin
         declare
            reponame    : constant String := USS (identifier);
            uppername   : constant String := uppercase (reponame);
            invpriority : constant Natural := Natural (Repo_priority'Last - priority);
            payload     : constant String := zeropad (invpriority, 2) & uppername & sep & reponame;
         begin
            temp_list.Append (SUS (payload));
         end;
      end populate_temp_list;

      procedure assemble_search_priority (Position : Pkgtypes.Text_List.Cursor)
      is
         encodingtxt : Text renames Pkgtypes.Text_List.Element (Position);
         delimiter   : constant String (1 .. 1) := (others => sep);
         repo_name   : constant String := part_2 (USS (encodingtxt), delimiter);
      begin
         remote_repositories.search_order.Append (SUS (repo_name));
      end assemble_search_priority;

   begin
      remote_repositories.repositories.Iterate(populate_temp_list'Access);
      Pkgtypes.sorter.Sort (temp_list);
      temp_list.Iterate (assemble_search_priority'Access);
   end define_search_priority;


   -----------------------------------
   --  convert_repo_configs_to_ucl  --
   -----------------------------------
   procedure convert_repo_configs_to_ucl
     (remote_repositories : A_Repo_Config_Set;
      repo_tree           : in out ThickUCL.UclTree)
   is
      num_repos : constant Natural := Natural (remote_repositories.search_order.Length);

      function convert (original : Mirror_Type) return String is
      begin
         case original is
            when not_mirrored => return "NONE";
            when SRV_mirror   => return "SRV";
         end case;
      end convert;

      function convert (original : Signature_Type) return String is
      begin
         case original is
            when not_signed    => return "NONE";
            when public_key    => return "PUBKEY";
            when fingerprinted => return "FINGERPRINTS";
         end case;
      end convert;

      function convert (original : IP_support) return Ucl.ucl_integer is
      begin
         case original is
            when no_restriction => return 0;
            when IPv4_only      => return 4;
            when IPv6_only      => return 6;
         end case;
      end convert;

      procedure set_environ (Position : Pkgtypes.NV_Pairs.Cursor)
      is
         name_txt : Text renames Pkgtypes.NV_Pairs.Key (Position);
         val_txt  : Text renames Pkgtypes.NV_Pairs.Element (Position);
      begin
         repo_tree.insert (USS (name_txt), USS (val_txt));
      end set_environ;
   begin
      repo_tree.start_object ("Repositories");
      for x in 0 .. num_repos - 1 loop
         declare
            index : Text renames remote_repositories.search_order.Element (x);
            repo_name : constant String := USS (index);
            repo : A_Repo_Config renames remote_repositories.repositories.Element (index);
         begin
            repo_tree.start_object (repo_name);
            repo_tree.insert ("master", repo.master);
            repo_tree.insert ("priority", Ucl.ucl_integer (repo.priority));
            repo_tree.insert ("connect_order", Ucl.ucl_integer (x + 1));
            repo_tree.insert ("url", USS (repo.url));
            repo_tree.insert ("mirror_type", convert (repo.mirror));
            repo_tree.insert ("signature_type", convert (repo.verification));
            if not IsBlank (repo.pubkey_path) then
               repo_tree.insert ("pubkey", USS (repo.pubkey_path));
            end if;
            if not IsBlank (repo.fprint_dir) then
               repo_tree.insert ("fingerprints", USS (repo.fprint_dir));
            end if;
            case repo.protocol is
               when no_restriction => null;
               when others =>
                  repo_tree.insert ("ip_version", convert (repo.protocol));
            end case;
            if not repo.environ_set.Is_Empty then
               repo_tree.start_object ("env");
               repo.environ_set.Iterate (set_environ'Access);
               repo_tree.close_object;
            end if;
            if not IsBlank (repo.scp_private) then
               repo_tree.insert ("scp_private_key", USS (repo.scp_private));
            end if;
            if not IsBlank (repo.scp_public) then
               repo_tree.insert ("scp_public_key", USS (repo.scp_public));
            end if;
            repo_tree.close_object;
         end;
      end loop;
      repo_tree.close_object;
   end convert_repo_configs_to_ucl;


   -----------------------------
   --  fetch_master_checksum  --
   -----------------------------
   function fetch_master_checksum
     (mirrors : A_Repo_Config_Set;
      forced  : Boolean;
      quiet   : Boolean) return Boolean
   is
      --  Don't use etag for master checksum
      --  Set the mtime of the cached checksum file for 5 minute in the future
      --  At beginning of routine, check for existing cached version.  If its mtime is
      --  in the future, use the cached version.
      --  With a file smaller than the etag header, using etag costs more than just fetching
      --  it again.
      cached_copy : constant String := downloaded_file_path (catalog_digest);
      result      : Fetch.fetch_result;
   begin
      --  ensure cache directory is available
      Ada.Directories.Create_Path (cache_directory);

      if CAL.target_file_cached (cached_copy) then
         if forced then
            if not Archive.Unix.unlink_file (cached_copy) then
               Event.emit_debug (high_level, "Failed to delete " & cached_copy);
            end if;
         else
            return True;
         end if;
      end if;

      if mirrors.repositories.Is_Empty then
         Event.emit_error ("No repositories available from which to fetch checksum");
         return False;
      end if;

      declare
         master_repo : constant String := USS (mirrors.master_repository) & " repository";
         url_str : constant String := master_url (catalog_digest, mirrors);
      begin
         if not valid_scheme_specified (url_str) then
            Event.emit_error ("The scheme specified in '" & url_str & "' is invalid.");
            return False;
         end if;

         result := Fetch.download_file
           (remote_file_url => url_str,
            etag_file       => "",
            downloaded_file => cached_copy,
            remote_repo     => True,
            remote_protocol => master_protocol (mirrors),
            remote_prv_key  => master_scp_private_key (mirrors),
            remote_pub_key  => master_scp_public_key (mirrors));

         case result is
            when Fetch.cache_valid | Fetch.file_downloaded =>
               if not quiet then
                  Event.emit_message ("Reference checksum obtained from the " & master_repo);
               end if;
               --  Set expiration time for 5 minutes in the future
               if not CAL.set_expiration_time (cached_copy, 300) then
                  Event.emit_debug (high_level, "Failed to update mtime on " & cached_copy);
               end if;
               return True;
            when Fetch.retrieval_failed =>
               Event.emit_error ("Failed to obtain reference checksum from the " & master_repo);
               return False;
         end case;
      end;

   end fetch_master_checksum;


   -----------------------
   --  cache_directory  --
   -----------------------
   function cache_directory return String is
   begin
      return Context.reveal_cache_directory & "/remote";
   end cache_directory;


   ----------------------------
   --  downloaded_etag_path  --
   ----------------------------
   function downloaded_etag_path (downfile : download_type) return String is
   begin
      case downfile is
         when catalog_digest  => return cache_directory & "/digest.etag";  -- not used
         when catalog_archive => return cache_directory & "/rvn-archive.etag";
      end case;
   end downloaded_etag_path;


   --------------------------
   --  downloaded_file_path  --
   --------------------------
   function downloaded_file_path (downfile : download_type) return String is
   begin
      case downfile is
         when catalog_digest  => return cache_directory & "/catalog.sum";
         when catalog_archive => return cache_directory & "/catalog.rvn";
      end case;
   end downloaded_file_path;


   --------------------
   --  master_url  --
   --------------------
   function master_url (downfile : download_type; mirrors : A_Repo_Config_Set) return String is
   begin
      if not mirrors.master_assigned then
         raise zero_repositories_configured;
      end if;

      declare
         base_url : constant String :=
           USS (mirrors.repositories.Element (mirrors.master_repository).url);
      begin
         case downfile is
            when catalog_digest  => return base_url & "/catalog.sum";
            when catalog_archive => return base_url & "/catalog.rvn";
         end case;
      end;
   end master_url;


   -----------------------
   --  master_protocol  --
   -----------------------
   function master_protocol (mirrors : A_Repo_Config_Set) return IP_support is
   begin
      if not mirrors.master_assigned then
         raise zero_repositories_configured;
      end if;
      return mirrors.repositories.Element (mirrors.master_repository).protocol;
   end master_protocol;


   ------------------------------
   --  master_scp_private_key  --
   ------------------------------
   function master_scp_private_key (mirrors : A_Repo_Config_Set) return String is
   begin
      if not mirrors.master_assigned then
         raise zero_repositories_configured;
      end if;
       return USS (mirrors.repositories.Element (mirrors.master_repository).scp_private);
   end master_scp_private_key;


   -----------------------------
   --  master_scp_public_key  --
   -----------------------------
   function master_scp_public_key (mirrors : A_Repo_Config_Set) return String is
   begin
      if not mirrors.master_assigned then
         raise zero_repositories_configured;
      end if;
      return USS (mirrors.repositories.Element (mirrors.master_repository).scp_public);
   end master_scp_public_key;


   -----------------------------
   --  latest_catalog_digest  --
   -----------------------------
   function read_catalog_digest return Blake_3.blake3_hash_hex
   is
      --  Ensure file exists before calling this function

      cached_copy : constant String := downloaded_file_path (catalog_digest);
   begin
      declare
         contents : constant String := CAL.file_to_string (cached_copy);
         line_one : constant String := CAL.first_line (contents);
      begin
         if line_one'Length = Blake_3.blake3_hash_hex'Length then
            return Blake_3.blake3_hash_hex (line_one);
         end if;
      end;
      raise invalid_catalog_digest;
   end read_catalog_digest;


   --------------------------------
   --  obtain_reference_catalog  --
   --------------------------------
   function obtain_reference_catalog (mirrors : A_Repo_Config_Set;
                                      forced  : Boolean;
                                      quiet   : Boolean) return Boolean
   is
      catalog_digest : Blake_3.blake3_hash_hex;
      cached_copy    : String := downloaded_file_path (catalog_archive);
      site_index     : Natural;
   begin
      if not fetch_master_checksum (mirrors, forced, quiet) then
         return False;
      end if;

      begin
         catalog_digest := read_catalog_digest;
      exception
         when invalid_catalog_digest =>
            Event.emit_error ("Reference checksum is not 64 characters long");
            return False;
      end;

      if not forced then
         if file_verified (cached_copy, catalog_digest) then
            --  Assume extracted contents still intact in the case
            if not quiet then
               Event.emit_message ("Cached catalog is still valid.");
            end if;
            return True;
         end if;
      end if;

      if fetch_from_mirror (mirrors, "catalog.rvn", cached_copy, catalog_digest, quiet,
                            site_index)
      then
         --  Retrieval of catalog.rvn was successful.  Now we need to extract it.
         if not extract_catalog_contents then
            return False;
         end if;
         if catalog_is_authentic (mirrors, site_index, quiet) then
            return True;
         end if;
         remove_catalog_files;
      end if;

      Event.emit_error ("Failed to retrieve the catalog archive.");
      return False;

   end obtain_reference_catalog;


   -------------------------
   --  fetch_from_mirror  --
   -------------------------
   function fetch_from_mirror (mirrors        : A_Repo_Config_Set;
                               relative_path  : String;
                               cache_location : String;
                               digest         : Blake_3.blake3_hash_hex;
                               quiet          : Boolean;
                               site_used      : out Natural) return Boolean
   is
      number_sites : constant Natural := Natural (mirrors.search_order.Length);
   begin
      for site_index in 0 .. number_sites - 1 loop
         site_used := site_index;
         declare
            index : Text renames mirrors.search_order.Element (site_index);
            repo : A_Repo_Config renames mirrors.repositories (index);
            result : Fetch.fetch_result;
            from_repo : constant String := " from the " & USS (repo.identifier) & " repository";
            url_str   : constant String := USS (repo.url);
         begin
            Event.emit_debug (high_level, "Selecting " & USS (repo.identifier) & " repository");
            if not valid_scheme_specified (url_str) then
               Event.emit_error ("The scheme specified in '" & url_str & "' is invalid.");
               return False;
            end if;
            case repo.mirror is
               when not_mirrored => null;
               when SRV_mirror =>
                  Event.emit_error ("SRV mirrors are currently unimplemented.");
                  return False;
            end case;
            result := Fetch.download_file (remote_file_url => url_str & "/" & relative_path,
                                           etag_file       => "",
                                           downloaded_file => cache_location,
                                           remote_repo     => True,
                                           remote_protocol => repo.protocol,
                                           remote_prv_key  => USS (repo.scp_private),
                                           remote_pub_key  => USS (repo.scp_public));
            case result is
               when Fetch.cache_valid | Fetch.file_downloaded =>
                  if file_verified (cache_location, digest) then
                     if not quiet then
                        Event.emit_message ("Downloaded and verified " & relative_path & from_repo);
                     end if;
                     return True;
                  else
                     if not quiet then
                        Event.emit_message ("Downloaded " & relative_path & from_repo &
                                              ", but the verification failed.");
                     end if;
                  end if;
               when Fetch.retrieval_failed =>
                  if not quiet then
                     Event.emit_message ("Download of " & relative_path & from_repo & "failed.");
                  end if;
            end case;
         end;
      end loop;
      return False;
   end fetch_from_mirror;


   ---------------------
   --  file_verified  --
   ---------------------
   function file_verified
     (file_path : String;
      digest : Blake_3.blake3_hash_hex) return Boolean
   is
      features : Archive.Unix.File_Characteristics;
      actual_digest : Blake_3.blake3_hash_hex;
   begin
      features := Archive.Unix.get_charactistics (file_path);
      case features.ftype is
         when Archive.regular =>
            actual_digest := Blake_3.hex (Blake_3.file_digest (file_path));
            return actual_digest = digest;
         when others => return False;
      end case;
   end file_verified;


   -------------------------------------
   --  create_local_catalog_database  --
   -------------------------------------
   function create_local_catalog_database
     (remote_repositories  : A_Repo_Config_Set;
      forced               : Boolean;
      quiet                : Boolean;
      from_catalog_command : Boolean := False) return Boolean
   is
      num_records : Natural;
      success     : Boolean;
   begin
      if not from_catalog_command then
         if not RCU.config_setting (RCU.CFG.autoupdate) then
            return True;
         end if;
      end if;

      if obtain_reference_catalog (remote_repositories, forced, quiet) then
         success := Catalog.generate_database (num_records);
         if success then
            if not quiet then
               Event.emit_message ("Catalog update completed. " &
                                     Strings.int2str (num_records) & " packages processed.");
            end if;
            return True;
         end if;
      end if;
      return False;
   end create_local_catalog_database;


   -------------
   --  clean  --
   -------------
   procedure clean (filename : String) is
   begin
      if Archive.Unix.file_exists (filename) then
         if not Archive.Unix.unlink_file (filename) then
            Event.emit_debug (high_level, "failed to unlink " & filename);
         end if;
      end if;
   end clean;


   --------------------------------
   --  extract_catalog_contents  --
   --------------------------------
   function extract_catalog_contents return Boolean
   is
      operation       : Archive.Unpack.Darc;
      level           : constant Archive.info_level := Archive.silent;
      archive_path    : constant String := downloaded_file_path (catalog_archive);
      good_extraction : Boolean;
   begin
      --  Remove old files in case new archives have less files
      clean (cache_directory & "/catalog.ucl");
      clean (cache_directory & "/signature");
      clean (cache_directory & "/repository.pub");

      operation.open_rvn_archive (archive_path, level);
      good_extraction := operation.extract_archive
        (top_directory => cache_directory,
         set_owners    => True,
         set_perms     => True,
         set_modtime   => True,
         skip_scripts  => True,
         upgrading     => False);
      operation.close_rvn_archive;

      if not good_extraction then
         Event.emit_error ("Failed to extract " & archive_path & " into " & cache_directory);
         return False;
      end if;

      return True;
   end extract_catalog_contents;


   ----------------------------
   --  remove_catalog_files  --
   ----------------------------
   procedure remove_catalog_files is
   begin
      clean (cache_directory & "/catalog.rvn");
      clean (cache_directory & "/catalog.sum");
      clean (cache_directory & "/catalog.ucl");
      clean (cache_directory & "/signature");
      clean (cache_directory & "/repository.pub");
   end remove_catalog_files;


   -----------------------------
   --  verify_signed_catalog  --
   -----------------------------
   function verify_signed_catalog
     (signature_file : String;
      key_path  : String;
      catalog   : String;
      quiet     : Boolean) return Boolean
   is
      c_key_path  : IC.Strings.chars_ptr;
      c_sig_path  : IC.Strings.chars_ptr;
      c_hash_len  : IC.size_t := IC.size_t (Blake_3.blake3_hash'Length);
      c_hash      : array (Blake_3.blake3_hash'Range) of aliased IC.unsigned_char;
      a_digest    : Blake_3.blake3_hash;
      result      : IC.int;

      use type IC.int;
   begin
      a_digest := Blake_3.file_digest (catalog);
      c_key_path := IC.Strings.New_String (key_path);
      c_sig_path := IC.Strings.New_String (signature_file);
      for x in Blake_3.blake3_hash'Range loop
         c_hash (x) := IC.unsigned_char (Character'Pos (a_digest (x)));
      end loop;

      result := Unix.C_Verify_Digest
        (hash     => c_hash (Blake_3.blake3_hash'First)'Access,
         hash_len => c_hash_len,
         key_path => c_key_path,
         sig_path => c_sig_path);

      IC.Strings.Free (c_key_path);
      IC.Strings.Free (c_sig_path);

      if result /= 0 then
         Event.emit_debug (high_level, "Verification failed, RC =" & result'Img);
         if not quiet then
            Event.emit_message ("fatal: catalog archive cannot be authenticated.");
         end if;
         return False;
      end if;

      return True;

   end verify_signed_catalog;


   ----------------------------
   --  catalog_is_authentic  --
   ----------------------------
   function catalog_is_authentic
     (mirrors    : A_Repo_Config_Set;
      site_index : Natural;
      quiet      : Boolean) return Boolean
   is
      signature : constant String := cache_directory & "/signature";
      key_file  : constant String := cache_directory & "/repository.pub";
      catalog   : constant String := cache_directory & "/catalog.ucl";
      repo_name : Text renames mirrors.search_order.Element (site_index);
      repo      : A_Repo_Config renames mirrors.repositories.Element (repo_name);
      features  : Archive.Unix.File_Characteristics;
      fingerprints_defined : Boolean;
      public_key_defined   : Boolean;
      found_signature      : Boolean;
      found_key_file       : Boolean;

      function error_msg (rsa_key_type : String) return String is
      begin
         return "Authentication is set to " & rsa_key_type & " by the " & USS (repo.identifier) &
           " repository, but the catalog is not signed.  Set SIGNATURE_TYPE to 'NONE' or check " &
           "with the maintainers if the repository should be signed.";
      end;
   begin
      fingerprints_defined := not IsBlank (repo.fprint_dir);
      public_key_defined   := not IsBlank (repo.pubkey_path);
      found_signature      := Archive.Unix.file_exists (signature);
      found_key_file       := Archive.Unix.file_exists (key_file);

      --  if authentication is unsigned, ignore pubkey and fingerprint definitions
      --  if authentication is fingerprints, it must be defined (files checked later)
      --  if authentication is public key, it must be defined and key path must be defined
      case repo.verification is
         when not_signed => null;
         when public_key =>
            if not public_key_defined then
               Event.emit_error
                 ("The " & USS (repo.identifier) & " repository authentication is set to " &
                    "use a public key, but PUBKEY is undefined.");
               return False;
            end if;
            features := Archive.Unix.get_charactistics (USS (repo.pubkey_path));
            case features.ftype is
               when Archive.regular => null;
               when others =>
                  Event.emit_error
                    ("The public key path in the " & USS (repo.identifier) & " repository " &
                       "configuration is not an existing file. Please install the public " &
                       "key at the indicated location.");
                  return False;
            end case;
         when fingerprinted =>
            if not fingerprints_defined then
               Event.emit_error
                 ("The " & USS (repo.identifier) & " repository authentication is set to " &
                    "use fingerprints, but FINGERPRINTS is undefined.");
               return False;
            end if;
            features := Archive.Unix.get_charactistics (USS (repo.fprint_dir));
            case features.ftype is
               when Archive.directory => null;
               when others =>
                  Event.emit_error
                    ("The fingerprints path in the " & USS (repo.identifier) & " repository " &
                       "configuration is not an existing directory.  Please install the " &
                       "fingerprint file in 'trusted' subdirectory.");
                  return False;
            end case;
      end case;

      --  nominal unsigned case: no signature file
      --  check for bad case where a signature is present but authentication is not set
      --  check for bad case where authentication is set but the signature is not present
      case repo.verification is
         when not_signed =>
            if found_signature then
               Event.emit_error
                 ("The catalog archive contains a signature file, but SIGNATURE_TYPE is " &
                    "not defined by the " & USS (repo.identifier) & " repository.");
               return False;
            else
               Event.emit_debug (high_level, "Catalog is unsigned as repo expected [good]");
               return True;
            end if;
         when public_key =>
            if not found_signature then
               Event.emit_error (error_msg ("public key"));
               return False;
            end if;
         when fingerprinted =>
            if not found_signature then
               Event.emit_error (error_msg ("fingerprints"));
               return False;
            end if;
            if not found_key_file then
               Event.emit_error
                 ("The " & USS (repo.identifier) & " repository set SIGNATURE_TYPE to " &
                    "'fingerprints' but the public key was not provided.  Please switch " &
                    "to public key verification or contact the maintainers to correct " &
                    "their signature method.");
            return False;
            end if;
      end case;

      --  handle key verification
       case repo.verification is
         when not_signed => return False;  --  impossible
         when fingerprinted =>
            declare
               --  Calculate digest of provided public key
               kf_digest : constant Blake_3.blake3_hash_hex :=
                 Blake_3.hex (Blake_3.file_digest (key_file));
               revoke_msg : constant String :=
                 "The fingerprints defined in the " & USS (repo.identifier) & " repository " &
                 "configuration has been revoked.  Please obtain trusted fingerprints " &
                 "and try again.";
               not_found_msg : constant String :=
                 "The fingerprints defined in the " & USS (repo.identifier) & " repository " &
                 "configuration do not correspond with the public key provided with the " &
                 "catalog. Please ensure the trusted fingerprint files are located correctly.";
            begin
               if confirm_matching_fingerprints (kf_digest, USS (repo.fprint_dir), trusted) then
                  --  provided public key is legit.
                  return verify_signed_catalog (signature, key_file, catalog, quiet);
               end if;
               if confirm_matching_fingerprints (kf_digest,  USS (repo.fprint_dir), revoked) then
                  Event.emit_error (revoke_msg);
                  return False;
               end if;
               Event.emit_error (not_found_msg);
               return False;
            end;
         when public_key =>
            return verify_signed_catalog (signature, USS (repo.pubkey_path), catalog, quiet);
      end case;

   end catalog_is_authentic;


   -------------------------------------
   --  confirm_matching_fingerprints  --
   -------------------------------------
   function confirm_matching_fingerprints
     (key_file_digest : Blake_3.blake3_hash_hex;
      fprints_path    : String;
      status          : fprint_status) return Boolean
   is
      --  check <fprints_path>/trusted when status = trusted
      --  check <fprints_path>/revoked when status = trusted
      --  Search directory for files within.
      --  Parse each one with UCL parser
      --  If parse succeeds, verify "function" defined as "blake3" and "fingerprint" is defined.
      --  If the value of "fingerprint" matches <key_file_digest> return true.
      --  Return false when loop ends unsuccessfully.

      file_list   : DSC.dscan_crate.Vector;
      match_found : Boolean := False;
      features    : Archive.Unix.File_Characteristics;

      function dir_path return String is
      begin
         case status is
            when trusted => return fprints_path & "/trusted";
            when revoked => return fprints_path & "/revoked";
         end case;
      end dir_path;

      procedure check_fingerprint (position : DSC.dscan_crate.Cursor)
      is
         item : Archive.Dirent.Directory_Entity renames DSC.dscan_crate.Element (position);
         ftree : ThickUCL.UclTree;
      begin
         if not match_found then
            Event.emit_debug (high_level, "looking at " & item.full_path);
            ThickUCL.Files.parse_ucl_file (ftree, item.full_path, "");
            if not ftree.key_exists ("function") or else
              not ftree.key_exists ("fingerprint")
            then
               return;
            end if;
            declare
               algorithm   : constant String := lowercase (ftree.get_base_value ("function"));
               fingerprint : constant String := lowercase (ftree.get_base_value ("fingerprint"));
            begin
               if algorithm = "blake3" and then fingerprint = key_file_digest then
                  match_found := True;
               end if;
            end;
         end if;
      exception
         when ThickUCL.Files.ucl_file_unparseable =>
            Event.emit_debug (high_level, "check_fingerprint - unparsable: " & item.full_path);
      end check_fingerprint;

   begin
      features := Archive.Unix.get_charactistics (dir_path);
      case features.ftype is
         when Archive.directory =>
            DSC.scan_directory (dir_path, file_list);
            file_list.Iterate (check_fingerprint'Access);
         when others => null;
      end case;
      return match_found;

   end confirm_matching_fingerprints;


   ------------------------------
   --  valid_scheme_specified  --
   ------------------------------
   function valid_scheme_specified (url : String) return Boolean
   is
      url_delimiter : constant String := "://";
   begin
      if not contains (url, url_delimiter) then
         return False;
      end if;
      declare
         delim       : constant Character := Character'Val (0);
         delimstr    : constant String (1 .. 1) := (others => delim);
         scheme      : constant String := part_1 (url, url_delimiter);
         flat_values : constant String := RCU.config_setting_as_string (RCU.CFG.valid_scheme);
         num_fields  : constant Natural := count_char (flat_values, delim) + 1;
      begin
         for x in 1 .. num_fields loop
            if specific_field (flat_values, x, delimstr) = scheme then
               return True;
            end if;
         end loop;
      end;
      return False;
   end valid_scheme_specified;

end Raven.Repository;
