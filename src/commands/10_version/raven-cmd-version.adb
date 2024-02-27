--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Version;
with Raven.Unix;
with Raven.Strings;
with Raven.Cmd.Unset;
with Raven.Event;
with Raven.Context;
with Raven.Fetch;
with curl_callbacks;

Use Raven.Strings;

package body Raven.Cmd.Version is

   package VER renames Raven.Version;
   package RCU renames Raven.Cmd.Unset;
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
     (pkg_version         : String;
      pkg_nsv             : String;
      source              : String;
      version             : String;
      option_match_status : Boolean;
      option_avoid_status : Boolean;
      option_verbose      : Boolean;
      option_cmp_operator : Character) return Display_Line
   is
      key  : Character;
      line : Display_Line;
   begin
      line.valid := False;
      if IsBlank (version) then
         if IsBlank (source) then
            key := '!';
         else
            key := '?';
         end if;
      else
         case VER.pkg_version_cmp (pkg_version, version) is
            when -1 => key := '<';
            when  0 => key := '=';
            when  1 => key := '>';
         end case;
      end if;

      if option_match_status and then option_cmp_operator /= key then
         --  skip records where the key does not match given character
         return line;
      end if;

      if option_avoid_status and then option_cmp_operator = key then
         --  skip records where the key matches given character
         return line;
      end if;

      line.comparison := key;
      --  This behavior differs from pkgng
      --  Display name-version always.  Displaying origin when search was origin-based
      --  no longer makes since with a many-to-1 subpackage relationship to origin.
      line.identifier := SUS (pkg_nsv & "-" & pkg_version);

      if option_verbose then
         case key is
            when '<' =>
               line.extra_info := SUS ("needs updating (" & source & " has " & version & ")");
            when '=' =>
               line.extra_info := SUS ("up-to-date with " & source);
            when '>' =>
               line.extra_info := SUS ("newer (" & source & " has " & version & ")");
            when '?' =>
               line.extra_info := SUS ("orphaned: " & pkg_nsv);
            when '!' =>
               line.extra_info := SUS ("Comparison failed");
            when others =>
               line.extra_info := SUS ("?????");
         end case;
      end if;
      line.valid := True;
      return line;
   end print_version;


   --------------------------------
   --  Executes version command  --
   --------------------------------
   function execute_version_command (comline : Cldata) return Boolean
   is
      type reference_source is (unset, release_index, snapshot_index, repo_catalog);
      reference : reference_source := unset;
   begin
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
            --  happens when no -t, -T, -S, -I, -R, or -r switch set
            declare
               versionsrc : constant String := RCU.config_setting (RCU.CFG.version_source);
            begin
               if versionsrc'Length > 0 then
                  case versionsrc (versionsrc'First) is
                     when 'S' => reference := snapshot_index;
                     when 'I' => reference := release_index;
                     when 'R' => reference := repo_catalog;
                     when others =>
                        Event.emit_notice
                          ("Invalid VERSION_SOURCE in configuration: " & versionsrc);
                        Event.emit_notice ("Using latest release index as the reference.");
                  end case;
               end if;
               if reference = unset then
                  --  Source unspecified, default to using the latest release index
                  reference := release_index;
               end if;
            end;
      end case;

      declare
         option_verbose      : Boolean := comline.common_options.verbose;
         option_name         : Boolean := not IsBlank (comline.cmd_version.pkg_name);
         option_match_status : Boolean := (comline.cmd_version.match_char /= Character'First);
         option_avoid_status : Boolean;
         option_cmp_operator : Character;

         --  match          : Database.Match_Behavior := Database.MATCH_ALL;
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
            when unset => return False;  --  Can't happen
            when release_index =>
               --  TODO
               return False;
            when snapshot_index =>
               --  TODO
               return False;
            when repo_catalog =>
               --  TODO
               return False;
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
      return Context.reveal_db_directory & "/version";
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


   -----------------------
   --  download_file_path  --
   -----------------------
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
   function index_database_path (downfile : download_type) return String
   is
      basedir : constant String := Context.reveal_db_directory & "/version";
   begin
      case downfile is
         when release  => return database_directory & "/rvnindex.sqlite";
         when snapshot => return database_directory & "/snapshot.sqlite";
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


end Raven.Cmd.Version;
