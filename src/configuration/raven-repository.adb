--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Exceptions;
with Raven.Cmd.Unset;
with Raven.Strings;
with Raven.Event;
with Archive.Unix;
with Archive.Misc;
with Archive.Dirent.Scan;
with ThickUCL.Files;
with Ucl;

use Raven.Strings;

package body Raven.Repository is

   package RCU renames Raven.Cmd.Unset;
   package DSC renames Archive.Dirent.Scan;


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
            invpriority : constant Natural := Natural (Repo_priority'Last - priority);
            payload     : constant String := zeropad (invpriority, 2) & reponame & sep & reponame;
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
            repo_tree.close_object;
         end;
      end loop;
      repo_tree.close_object;
   end convert_repo_configs_to_ucl;

end Raven.Repository;
