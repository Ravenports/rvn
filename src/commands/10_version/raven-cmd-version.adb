--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Directories;
with Raven.Version;
with Raven.Unix;
with Raven.Strings;
with Raven.Cmd.Unset;
with Raven.Event;
with Raven.Context;
with Raven.Fetch;
with Raven.Database.Lock;
with Raven.Database.Cmdversion;
with Raven.Database.Operations;
with Raven.Repository;
with curl_callbacks;
with Archive.Unix;

Use Raven.Strings;

package body Raven.Cmd.Version is

   package DIR renames Ada.Directories;
   package VER renames Raven.Version;
   package RCU renames Raven.Cmd.Unset;
   package LOK renames Raven.Database.Lock;
   package DBC renames Raven.Database.Cmdversion;
   package OPS renames Raven.Database.Operations;
   package CAL renames curl_callbacks;

   ----------------------
   --  do_testversion  --
   ----------------------
   function do_testversion (pkgname1, pkgname2 : String) return Boolean is
   begin
      case VER.pkg_version_cmp (pkgname1, pkgname2) is
         when -1 => TIO.Put_Line ("<");
         when  0 => TIO.Put_Line ("=");
         when  1 => TIO.Put_Line (">");
      end case;
      return True;
   end do_testversion;


   ----------------------
   --  do_testpattern  --
   ----------------------
   function do_testpattern
     (pkgname : String;
      pattern : String;
      hyphen1 : Boolean;
      hyphen2 : Boolean) return Boolean
   is
      result  : Boolean := False;
      tmpres  : Boolean;
      fields  : Natural;
      zeroasc : constant Character := Character'Val (0);
      zerostr : constant String (1 .. 1) := (1 => zeroasc);
   begin
      if hyphen1 then

         -- [FILTER MODE] many package names, one pattern
         fields := count_char (pkgname, zeroasc) + 1;
         for x in 1 .. fields loop
            declare
               name : constant String := specific_field (pkgname, x, zerostr);
            begin
               if name'Length > 0 then
                  tmpres := Unix.filename_match (pattern, name);
                  if tmpres then
                     result := True;
                     TIO.Put_Line (name);
                  end if;
               end if;
            end;
         end loop;

      elsif hyphen2 then

         -- [FILTER MODE] one package name, many patterns
         fields := count_char (pattern, zeroasc) + 1;
         for x in 1 .. fields loop
            declare
               subpattern : constant String := specific_field (pattern, x, zerostr);
            begin
               if subpattern'Length > 0 then
                  tmpres := Unix.filename_match (subpattern, pkgname);
                  if tmpres then
                     result := True;
                     TIO.Put_Line (subpattern);
                  end if;
               end if;
            end;
         end loop;

      else

         result := Unix.filename_match (pattern, pkgname);

      end if;
      return result;
   end do_testpattern;


   ---------------------
   --  print_version  --
   ---------------------
   function print_version
     (installed_nsv       : String;
      installed_version   : String;
      source              : A_Source;
      remote_version      : String;
      option_match_status : Boolean;
      option_avoid_status : Boolean;
      option_verbose      : Boolean;
      option_cmp_operator : Character) return Display_Line
   is
      function remote_src return String is
      begin
         case source is
            when S_snapshot_index => return "ports snapshot";
            when I_release_index  => return "ports release";
            when R_remote_catalog => return "packages catalog";
         end case;
      end remote_src;

      key  : Character := '?';   --  Default: Orphaned (blank remote version)
      line : Display_Line;
   begin
      line.valid := False;
      line.identifier := SUS (installed_nsv);
      line.extra_info := SU.Null_Unbounded_String;
      if not IsBlank (remote_version) then
         case VER.pkg_version_cmp (installed_version, remote_version) is
            when -1 => key := '<';
            when  0 => key := '=';
            when  1 => key := '>';
         end case;
      end if;
      line.comparison := key;

      if option_match_status and then option_cmp_operator /= key then
         --  skip records where the key does not match given character
         return line;
      end if;

      if option_avoid_status and then option_cmp_operator = key then
         --  skip records where the key matches given character
         return line;
      end if;

      if option_verbose then
         case key is
            when '<' =>
               line.extra_info := SUS ("behind " & remote_src & " (" & installed_version &
                                         " < " & remote_version & ")");
            when '=' =>
               line.extra_info := SUS ("latest version (" & installed_version & ")");
            when '>' =>
               line.extra_info := SUS ("ahead of " & remote_src & " (" & installed_version &
                                         " > " & remote_version & ")");
            when '?' =>
               line.extra_info := SUS ("orphaned, package not present in " & remote_src);
            when others =>
               null;
         end case;
      end if;
      line.valid := True;
      return line;
   end print_version;


   --------------------------------
   --  compare_against_rvnindex  --
   --------------------------------
   function compare_against_rvnindex
     (source              : download_type;
      option_match_status : Boolean;
      option_avoid_status : Boolean;
      option_verbose      : Boolean;
      behave_cs           : Boolean;
      behave_exact        : Boolean;
      option_cmp_operator : Character;
      pattern             : String) return Boolean
   is
      remote_versions     : Pkgtypes.NV_Pairs.Map;
      local_installation  : Pkgtypes.NV_Pairs.Map;
      localdb             : Database.RDB_Connection;
      source2             : A_Source;

      procedure print (Position : Pkgtypes.NV_Pairs.Cursor)
      is
         nsv_key : Text renames Pkgtypes.NV_Pairs.Key (Position);
         remote_version : Text := SU.Null_Unbounded_String;
         line : Display_Line;
      begin
         if remote_versions.Contains (nsv_key) then
            remote_version := remote_versions.Element (nsv_key);
         end if;

         line := print_version (installed_nsv       => USS (nsv_key),
                                installed_version   => USS (Pkgtypes.NV_Pairs.Element (Position)),
                                source              => source2,
                                remote_version      => USS (remote_version),
                                option_match_status => option_match_status,
                                option_avoid_status => option_avoid_status,
                                option_verbose      => option_verbose,
                                option_cmp_operator => option_cmp_operator);
         if not line.valid then
            return;
         end if;

         if option_verbose then
            Event.emit_message (line.comparison & ' ' & pad_right (USS (line.identifier), 48) &
                                  USS (line.extra_info));
         else
            Event.emit_message (line.comparison & ' ' & USS (line.identifier));
         end if;
      end print;
   begin
      case source is
         when reldate =>
            Event.emit_debug (high_level,
                              "Developer error: Invalid compare_against_rvnindex index_type");
            return False;
         when snapshot =>
            source2 := S_snapshot_index;
         when release =>
            source2 := I_release_index;
      end case;
      if not create_index_database (source) then
         return False;
      end if;

      DBC.map_nsv_to_rvnindex_version
        (database_directory => database_directory,
         database_file_path => index_database_path (source),
         version_map        => remote_versions);

      case OPS.rdb_open_localdb (localdb, Database.installed_packages) is
         when RESULT_OK => null;
         when others => return False;
      end case;

      if not LOK.obtain_lock (localdb, LOK.lock_readonly) then
         Event.emit_error (LOK.no_read_lock);
         OPS.rdb_close (localdb);
         return False;
      end if;

      DBC.map_nsv_to_local_version
        (db           => localdb,
         behave_cs    => behave_cs,
         behave_exact => behave_exact,
         pattern      => pattern,
         version_map  => local_installation);

      OPS.rdb_close (localdb);

      if not LOK.release_lock (localdb, LOK.lock_readonly) then
         Event.emit_error (LOK.no_read_unlock);
         OPS.rdb_close (localdb);
         return False;
      end if;

      local_installation.Iterate (print'Access);
      return True;

   end compare_against_rvnindex;


   -------------------------------
   --  compare_against_catalog  --
   -------------------------------
   function compare_against_catalog
     (single_repo         : String;
      option_match_status : Boolean;
      option_avoid_status : Boolean;
      option_verbose      : Boolean;
      behave_cs           : Boolean;
      behave_exact        : Boolean;
      option_cmp_operator : Character;
      pattern             : String) return Boolean
   is
      remote_versions     : Pkgtypes.NV_Pairs.Map;
      local_installation  : Pkgtypes.NV_Pairs.Map;
      localdb             : Database.RDB_Connection;
      rdb                 : Database.RDB_Connection;

      procedure print (Position : Pkgtypes.NV_Pairs.Cursor)
      is
         nsv_key : Text renames Pkgtypes.NV_Pairs.Key (Position);
         remote_version : Text := SU.Null_Unbounded_String;
         line : Display_Line;
      begin
         if remote_versions.Contains (nsv_key) then
            remote_version := remote_versions.Element (nsv_key);
         end if;

         line := print_version (installed_nsv       => USS (nsv_key),
                                installed_version   => USS (Pkgtypes.NV_Pairs.Element (Position)),
                                source              => R_remote_catalog,
                                remote_version      => USS (remote_version),
                                option_match_status => option_match_status,
                                option_avoid_status => option_avoid_status,
                                option_verbose      => option_verbose,
                                option_cmp_operator => option_cmp_operator);
         if not line.valid then
            return;
         end if;

         if option_verbose then
            Event.emit_message (line.comparison & ' ' & pad_right (USS (line.identifier), 48) &
                                  USS (line.extra_info));
         else
            Event.emit_message (line.comparison & ' ' & USS (line.identifier));
         end if;
      end print;
   begin
      if not refresh_catalog (single_repo) then
         return False;
      end if;

      case OPS.rdb_open_localdb (rdb, Database.catalog) is
         when RESULT_OK => null;
         when others => return False;
      end case;

      if not LOK.obtain_lock (rdb, LOK.lock_readonly) then
         Event.emit_error (LOK.no_read_lock);
         OPS.rdb_close (rdb);
         return False;
      end if;

      DBC.map_nsv_to_local_version (rdb, False, False, "", remote_versions);

      if not LOK.release_lock (rdb, LOK.lock_readonly) then
         Event.emit_error (LOK.no_read_unlock);
         OPS.rdb_close (rdb);
         return False;
      end if;

      OPS.rdb_close (rdb);

      case OPS.rdb_open_localdb (localdb, Database.installed_packages) is
         when RESULT_OK => null;
         when others => return False;
      end case;

      if not LOK.obtain_lock (localdb, LOK.lock_readonly) then
         Event.emit_error (LOK.no_read_lock);
         OPS.rdb_close (localdb);
         return False;
      end if;

      DBC.map_nsv_to_local_version (localdb, behave_cs, behave_exact, pattern, local_installation);

      if not LOK.release_lock (localdb, LOK.lock_readonly) then
         Event.emit_error (LOK.no_read_unlock);
         OPS.rdb_close (localdb);
         return False;
      end if;

      OPS.rdb_close (localdb);

      local_installation.Iterate (print'Access);
      return True;

   end compare_against_catalog;


   --------------------------------
   --  Executes version command  --
   --------------------------------
   function execute_version_command (comline : Cldata) return Boolean
   is
      type reference_source is (unset, release_index, snapshot_index, repo_catalog);
      reference : reference_source := unset;
      behave_cs  : Boolean := comline.common_options.case_sensitive;
   begin
      if Context.reveal_case_sensitive then
         behave_cs := True;
      end if;

      case comline.cmd_version.behavior is
         when test_versions =>
            return do_testversion (pkgname1 => USS (comline.cmd_version.test1),
                                   pkgname2 => USS (comline.cmd_version.test2));
         when compare_against_pattern =>
            return do_testpattern (pkgname => USS (comline.cmd_version.test1),
                                   pattern => USS (comline.cmd_version.test2),
                                   hyphen1 => comline.cmd_version.hyphen1,
                                   hyphen2 => comline.cmd_version.hyphen2);
         when use_rvnindex_release =>
            reference := release_index;
         when use_rvnindex_snapshot =>
            reference := snapshot_index;
         when use_remote_catalog_state =>
            reference := repo_catalog;
         when no_defined_behavior =>
            --  happens when no -t, -T, -S, -I, -R switch set
            declare
               versionsrc : constant String := RCU.config_setting (RCU.CFG.version_source);
            begin
               if versionsrc'Length > 0 then
                  case versionsrc (versionsrc'First) is
                     when 'S' | 's' => reference := snapshot_index;
                     when 'I' | 'i' => reference := release_index;
                     when 'R' | 'r' => reference := repo_catalog;
                     when others =>
                        Event.emit_message
                          ("Invalid VERSION_SOURCE in configuration: " & versionsrc);
                        Event.emit_message ("Using latest release index as the reference.");
                  end case;
               end if;
            end;
            if reference = unset then
               --  Source unspecified, default to using the latest release index
               reference := release_index;
            end if;
      end case;

      if not Archive.Unix.user_is_root then
         Event.emit_error ("This function of the version command is restricted to the superuser.");
         return False;
      end if;

      declare
         option_verbose      : Boolean := comline.common_options.verbose;
         option_match_status : Boolean := (comline.cmd_version.match_char /= Character'First);
         option_avoid_status : Boolean;
         option_cmp_operator : Character;
      begin
         --  -l/-L are mutually exclusive and both can't be set.
         if option_match_status then
            option_cmp_operator := comline.cmd_version.match_char;
            option_avoid_status := False;
         else
            option_avoid_status := (comline.cmd_version.not_char /= Character'First);
            if option_avoid_status then
               option_cmp_operator := comline.cmd_version.not_char;
            end if;
         end if;

         case reference is
            when unset =>
               return False;  --  Can't happen
            when release_index =>

               return compare_against_rvnindex
                    (source              => release,
                     option_match_status => option_match_status,
                     option_avoid_status => option_avoid_status,
                     option_verbose      => option_verbose,
                     behave_cs           => behave_cs,
                     behave_exact        => comline.common_options.exact_match,
                     option_cmp_operator => option_cmp_operator,
                     pattern             => USS (comline.common_options.name_pattern));

            when snapshot_index =>

               return compare_against_rvnindex
                    (source              => snapshot,
                     option_match_status => option_match_status,
                     option_avoid_status => option_avoid_status,
                     option_verbose      => option_verbose,
                     behave_cs           => behave_cs,
                     behave_exact        => comline.common_options.exact_match,
                     option_cmp_operator => option_cmp_operator,
                     pattern             => USS (comline.common_options.name_pattern));

            when repo_catalog =>

               return compare_against_catalog
                 (single_repo         => USS (comline.common_options.repo_name),
                  option_match_status => option_match_status,
                  option_avoid_status => option_avoid_status,
                  option_verbose      => option_verbose,
                  behave_cs           => behave_cs,
                  behave_exact        => comline.common_options.exact_match,
                  option_cmp_operator => option_cmp_operator,
                  pattern             => USS (comline.common_options.name_pattern));

         end case;
      end;

   end execute_version_command;


   -----------------------
   --  cache_directory  --
   -----------------------
   function cache_directory return String is
   begin
      return Context.reveal_cache_directory & "/version";
   end cache_directory;


   --------------------------
   --  database_directory  --
   --------------------------
   function database_directory return String is
   begin
      return Context.reveal_db_directory;
   end database_directory;


   -----------------
   --  index_url  --
   -----------------
   function index_url (downfile : download_type) return String
   is
      baseurl : constant string := "https://raw.githubusercontent.com/Ravenports/Ravenports/";
   begin
      case downfile is
         when release  =>
            declare
               url : constant String := latest_release_url;
            begin
               if url = DLOAD_FAILED then
                  Event.emit_message ("Failed to determine latest Ravenports release");
                  return DLOAD_FAILED;
               end if;
               return baseurl & url;
            end;
         when snapshot => return baseurl & "master/Mk/Misc/rvnindex.txt";
         when reldate  => return baseurl & "master/Mk/Misc/latest_release.txt";
      end case;
   end index_url;


   ---------------------------
   --  downloaded_etag_path  --
   ----------------------------
   function downloaded_etag_path (downfile : download_type) return String is
   begin
      case downfile is
         when release  => return cache_directory & "/rvnindex.etag";
         when snapshot => return cache_directory & "/snapshot.etag";
         when reldate  => return cache_directory & "/latest_release.etag";
      end case;
   end downloaded_etag_path;


   --------------------------
   --  download_file_path  --
   --------------------------
   function download_file_path (downfile : download_type) return String is
   begin
      case downfile is
         when release  => return cache_directory & "/rvnindex.txt";
         when snapshot => return cache_directory & "/snapshot.txt";
         when reldate  => return cache_directory & "/latest_release.txt";
      end case;
   end download_file_path;


   ---------------------------
   --  index_database_path  --
   ---------------------------
   function index_database_path (downfile : download_type) return String is
   begin
      case downfile is
         when release  => return database_directory & "/rvnindex_release.sqlite";
         when snapshot => return database_directory & "/rvnindex_snapshot.sqlite";
         when reldate  => return "/tmp/cannot_happen";
      end case;
   end index_database_path;


   --------------------------
   --  latest_release_url  --
   --------------------------
   function latest_release_url return String
   is
      downloaded_file : constant String := download_file_path (reldate);
   begin
      case Fetch.download_file
        (remote_file_url => index_url (reldate),
         etag_file       => downloaded_etag_path (reldate),
         downloaded_file => downloaded_file)
      is
         when Fetch.cache_valid | Fetch.file_downloaded =>
            declare
               contents   : constant String := CAL.file_to_string (downloaded_file);
               releasever : constant String := CAL.first_line (contents);
            begin
               return releasever & "/Mk/Misc/rvnindex.txt";
            end;
         when Fetch.retrieval_failed =>
            return DLOAD_FAILED;
      end case;
   end latest_release_url;


   -----------------------------
   --  create_index_database  --
   -----------------------------
   function create_index_database (index_type : download_type) return Boolean
   is
      rvnindex_file_path : constant String := download_file_path (index_type);
      database_file_path : constant String := index_database_path (index_type);
   begin
      DIR.Create_Path (cache_directory);

      case index_type is
         when reldate =>
            Event.emit_debug (high_level, "Invalid value for create_index_database index_type");
            return False;

         when snapshot | release =>
            declare
               remote_file_url : constant String := index_url (index_type);
            begin
               if remote_file_url = DLOAD_FAILED then
                  return False;
               end if;
               case Fetch.download_file
                 (remote_file_url => remote_file_url,
                  etag_file       => downloaded_etag_path (index_type),
                  downloaded_file => rvnindex_file_path)
               is
                  when Fetch.retrieval_failed =>
                     Event.emit_debug
                       (high_level, "Failed to return " & index_type'Img & " rvnindex from github");
                     return False;
                  when Fetch.cache_valid =>
                     if DIR.Exists (index_database_path (index_type)) then
                        Event.emit_debug (moderate, "Cached rvnindex database still valid.");
                        return True;
                     end if;
                     return DBC.create_rvnindex (database_directory => database_directory,
                                                 database_file_path => database_file_path,
                                                 rvnindex_file_path => rvnindex_file_path);
                  when Fetch.file_downloaded =>
                     if DIR.Exists (index_database_path (index_type)) then
                        Event.emit_debug (moderate, "Cached rvnindex database is obsolete.");
                     end if;
                     return DBC.create_rvnindex (database_directory => database_directory,
                                                 database_file_path => database_file_path,
                                                 rvnindex_file_path => rvnindex_file_path);
               end case;
            end;
      end case;
   end create_index_database;


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


end Raven.Cmd.Version;
