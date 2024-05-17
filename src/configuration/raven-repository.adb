--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Cmd.Unset;
with Raven.Strings;
with Raven.Event;
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
      remote_repositories : in out A_Repo_Config_Set)
   is
      conf_tree   : ThickUCL.UclTree;
      identifiers : ThickUCL.jar_string.Vector;
      expkeys     : constant String := expansion_keys;

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
                     value : constant Boolean := conf_tree.get_base_value (field_key);
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
                     value : constant Ucl.ucl_integer := conf_tree.get_base_value (field_key);
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
                     value : constant String := conf_tree.get_base_value (field_key);
                  begin
                     if upper_key = "URL" then
                        rconfig.url := SUS (value);
                     elsif upper_key = "PUBKEY" then
                        rconfig.pubkey_path := SUS (value);
                     elsif upper_key = "FINGERPRINTS" then
                        rconfig.fprint_dir := SUS (value);
                     elsif upper_key = "MIRROR_TYPE" then
                        if value = "SRV" then
                           rconfig.mirror := SRV_mirror;
                        end if;
                     elsif upper_key = "SIGNATURE_TYPE" then
                        if value = "PUBKEY" then
                           rconfig.verification := public_key;
                        elsif value = "FINGERPRINTS" then
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
                  remote_repositories.repositories.Insert (SUS (identifier), rconfig);
               end if;
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
   procedure load_repository_configurations (remote_repositories : in out A_Repo_Config_Set)
   is
      delim    : constant Character := Character'Val (0);
      delim2   : constant String (1 .. 1) := (others => delim);
      setting  : constant String := RCU.config_setting_as_string (RCU.CFG.repos_dir);
      numlines : constant Natural := count_char (setting, delim) + 1;

      procedure ingest_repository_config_file (Position : DSC.dscan_crate.Cursor)
      is
         file_path : constant String := DSC.dscan_crate.Element (Position).full_path;
      begin
         process_repository_configuration (file_path, remote_repositories);
      end ingest_repository_config_file;
   begin
      for line in 1 .. numlines loop
         declare
            dir_repo_config : constant String := specific_field (setting, line, delim2);
            files : DSC.dscan_crate.Vector;
         begin
            DSC.scan_directory (dir_repo_config, files);
            files.Iterate (ingest_repository_config_file'Access);
         end;
      end loop;
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

end Raven.Repository;
