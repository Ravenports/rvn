--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Raven.Strings; use Raven.Strings;
with Raven.Event;
with Ada.Strings.Hash;
with Ada.Characters.Latin_1;
with Ada.Environment_Variables;
with Archive.Unix;
with Ucl;
With ThickUCL.Files;


package body Raven.Configuration is

   package LAT renames Ada.Characters.Latin_1;
   package ENV renames Ada.Environment_Variables;
   package EV  renames Raven.Event;
   
   
   ------------------
   --  get_ci_key  --
   ------------------
   function get_ci_key (ci : Configuration_Item) return String
   is
   begin
      case ci is 
         when ci_not_found   => return "NOTFOUND";
         when dbdir          => return "DBDIR";
         when cachedir       => return "CACHEDIR";      
         when rc_scripts     => return "HANDLE_RC_SCRIPTS";
         when always_yes     => return "DEFAULT_ALWAYS_YES";
         when assume_yes     => return "ASSUME_ALWAYS_YES";
         when repos_dir      => return "REPOS_DIR";
         when keywords_dir   => return "KEYWORDS_DIR";
         when syslog         => return "SYSLOG";
         when abi            => return "ABI";
         when dev_mode       => return "DEVELOPER_MODE";
         when fetch_retry    => return "FETCH_RETRY";
         when fetch_timeout  => return "FETCH_TIMEOUT";
         when debug_scripts  => return "DEBUG_SCRIPTS";
         when permissive     => return "PERMISSIVE";
         when autoupdate     => return "REPO_AUTOUPDATE"; 
         when nameserver     => return "NAMESERVER";
         when user_agent     => return "HTTP_USER_AGENT";
         when event_pipe     => return "EVENT_PIPE";
         when no_timestamp   => return "UNSET_TIMESTAMP";
         when restrict_dir   => return "SSH_RESTRICT_DIR";
         when ssh_args       => return "SSH_ARGS";
         when environ        => return "RVN_ENVIRON";
         when debug_level    => return "DEBUG_LEVEL";
         when alias          => return "ALIAS";
         when run_scripts    => return "RUN_SCRIPTS";
         when case_match     => return "CASE_SENSITIVE_MATCH";
         when lock_wait      => return "LOCK_WAIT";
         when lock_retries   => return "LOCK_RETRIES";
         when sqlite_profile => return "SQLITE_PROFILE";
         when read_lock      => return "READ_LOCK";
         when automerge      => return "AUTOMERGE";
         when create_verbose => return "CREATE_VERBOSE";
         when autoclean      => return "AUTOCLEAN";
         when valid_scheme   => return "VALID_URL_SCHEME";
         when base_shlibs    => return "ALLOW_BASE_SHLIBS";
         when size_limit     => return "WARN_SIZE_LIMIT";
         when metalog_file   => return "METALOG";
         when skip_dev       => return "SKIP_DEV_SUBPKG";
         when skip_nls       => return "SKIP_NLS_SUBPKG";
         when skip_man       => return "SKIP_MAN_SUBPKG";
         when skip_doc       => return "SKIP_DOC_SUBPKG";
         when skip_examples  => return "SKIP_EXAM_SUBPKG";
      end case;
   end get_ci_key;
   
   
   -------------------
   --  get_ci_type  --
   -------------------
   function get_ci_type (ci : Configuration_Item) return ThickUCL.Leaf_type is
   begin
      case ci is
         when 
              rc_scripts     |
              always_yes     |
              assume_yes     |
              syslog         |
              dev_mode       |
              debug_scripts  |
              permissive     |
              autoupdate     |
              sqlite_profile |
              run_scripts    |
              case_match     |
              read_lock      |
              automerge      |
              create_verbose |
              autoclean      |
              no_timestamp   |
              skip_dev       |
              skip_doc       |
              skip_examples  |
              skip_man       |
              skip_nls       |
              base_shlibs    => return ThickUCL.data_boolean;
         when
              fetch_retry    |
              fetch_timeout  |
              debug_level    |
              lock_wait      |
              lock_retries   |
              size_limit     => return ThickUCL.data_integer;
         when
              dbdir          |
              cachedir       |
              keywords_dir   |
              abi            |
              nameserver     |
              user_agent     |
              event_pipe     |
              restrict_dir   |
              ssh_args       |
              metalog_file   => return ThickUCL.data_string;
         when
              repos_dir      |
              valid_scheme   => return ThickUCL.data_array;
         when
              environ        |
              alias          => return ThickUCL.data_object;
         when
              ci_not_found   => return ThickUCL.data_not_present;
      end case;
   end get_ci_type;
   
   
   ----------------------------
   --  get_default_value #1  --
   ----------------------------
   function get_default_value (ci : Configuration_Item) return Boolean is
   begin
      case ci is
         when rc_scripts     => return False;
         when always_yes     => return False;
         when assume_yes     => return False;
         when syslog         => return True;
         when dev_mode       => return False;
         when debug_scripts  => return False;
         when permissive     => return False;
         when autoupdate     => return True;
         when sqlite_profile => return False;
         when run_scripts    => return True;
         when case_match     => return False;
         when read_lock      => return False;
         when automerge      => return True;
         when create_verbose => return False;
         when autoclean      => return False;
         when no_timestamp   => return False;
         when base_shlibs    => return False;
         when skip_dev       => return True;
         when skip_nls       => return False;
         when skip_man       => return False;
         when skip_doc       => return False;       
         when skip_examples  => return True;
         when others => 
            raise config_type_mismatch with ci'Img & " is not of type boolean";
      end case;
   end get_default_value;

      
   ----------------------------
   --  get_default_value #2  --
   ----------------------------
   function get_default_value (ci : Configuration_Item) return Int64 is
   begin
      case ci is
         when fetch_retry    => return 3;
         when fetch_timeout  => return 30;
         when debug_level    => return 0;
         when lock_wait      => return 1;
         when lock_retries   => return 5;
         when size_limit     => return 1048576;
         when others => 
            raise config_type_mismatch with ci'Img & " is not of type integer";
      end case;
   end get_default_value;
   
   
   ----------------------------
   --  get_default_value #3  --
   ----------------------------
   function get_default_value (ci : Configuration_Item) return String is
   begin
      case ci is
         when dbdir          => return "/var/db/" & progname;
         when cachedir       => return "/var/cache/" & progname;
         when keywords_dir   => return "/var/ravenports/conspiracy/Mk/Keywords";
         when abi            => return "TODO - TBW";
         when nameserver     => return "";
         when user_agent     => return progname & "/" & progversion;
         when event_pipe     => return "";
         when restrict_dir   => return "";
         when ssh_args       => return "";
         when metalog_file   => return "";
         when others => 
            raise config_type_mismatch with ci'Img & " is not of type string";
      end case;
   end get_default_value;
   
   
   ----------------------------
   --  get_default_value #4  --
   ----------------------------
   procedure get_default_value (ci : Configuration_Item; crate : in out string_crate.Vector) is
   begin
      crate.Clear;
      case ci is
         when repos_dir =>
            crate.append (SUS ("/etc/" & progname));
            crate.Append (SUS (install_loc & "/etc/" & progname & "/repos"));
         when valid_scheme => 
            crate.Append (SUS ("https"));
            crate.Append (SUS ("http"));
            crate.Append (SUS ("file"));
            crate.Append (SUS ("ssh"));
            crate.Append (SUS ("tcp"));
         when others => 
            raise config_type_mismatch with ci'Img & " is not of type array of string.";
      end case;
   end get_default_value;
   
   
   ----------------------------
   --  get_default_value #5  --
   ----------------------------
   procedure get_default_value (ci : Configuration_Item; crate : in out object_crate.Map)
   is
   begin
      crate.Clear;
      case ci is
         when environ => 
            --  by default, the environment variants are empty
            null;  
         when alias =>
            --  The aliases are only defined in the /raven/etc/rvn.conf file
            null;
         when others =>
            raise config_type_mismatch with ci'Img & " is not of type UCL object";
      end case;
   end get_default_value;
   
   
   ----------------
   --  map_hash  --
   ----------------
   function map_hash (key : Text) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (USS (key));
   end map_hash;
   
   
   ------------------------------
   --  get_configuration_item  --
   ------------------------------
   function get_configuration_item (key : String) return Configuration_Item 
   is
   begin
      for ci in Configuration_Item'Range loop
         if get_ci_key (ci) = key then
            return ci;
         end if;
      end loop;
      return ci_not_found;
   end get_configuration_item;
   
   
   --------------------------------
   --  set_command_line_options  --
   --------------------------------
   procedure set_command_line_options 
     (options               : String;
      debug_level_cli       : A_Debug_Level;
      session_configuration : in out ThickUCL.UclTree)
   is
      --  if debug level is set by both -d command line option and the -o command line option,
      --  the -o value takes priority, although -d is in effect for the event emissions.
       numfields : Natural;
   begin
      if IsBlank (options) then
         return;
      end if;
      numfields := count_char (options, LAT.Vertical_Line) + 1;
      
      for F in 1 .. numfields loop
         declare
            delimiter : constant String := LAT.Vertical_Line & "";
            nvequals  : constant String := "=";
            nvpair    : constant String := specific_field (options, F, delimiter);
         begin
            if contains (nvpair, nvequals) then
               declare
                  name   : constant String := uppercase (part_1 (nvpair, nvequals));
                  val    : constant String := part_2 (nvpair, nvequals);
                  ci     : Configuration_Item;
                  citype : ThickUCL.Leaf_type;
               begin
                  ci := get_configuration_item (name);
                  citype := get_ci_type (ci);
                  case citype is
                     when ThickUCL.data_not_present => null;
                     when ThickUCL.data_string =>
                        session_configuration.insert (name, val);
                        ENV.Set (name, val);
                     when ThickUCL.data_integer =>
                        if IsNumeric (val) then
                           session_configuration.insert (name, Ucl.ucl_integer'Value (val));
                           ENV.Set (Name, val);
                        else
                           EV.emit_debug (high_level, "option value could not be converted " &
                                            "to integer, ignored.");
                        end if;
                     when ThickUCL.data_boolean =>
                        if IsBoolean (val) then
                           session_configuration.insert (name, Boolean'Value (val));
                           ENV.Set (Name, Uppercase (val));
                        else
                           EV.emit_debug (high_level, "option value could not be converted " &
                                            "to boolean, ignored.");
                        end if;
                     when ThickUCL.data_array =>
                        EV.emit_debug (high_level, "array option " & name & " can not be set " &
                                         "via the command line.");
                     when ThickUCL.data_object =>  
                        EV.emit_debug (high_level, "object option " & name & " can not be set " &
                                         "via the command line.");
                     when ThickUCL.data_time | ThickUCL.data_float =>
                        EV.emit_debug (high_level, "Impossible option " & name & " returns the " &
                                         "wrong type (float, time)");
                  end case;
               end;
            else
               EV.emit_debug (high_level, "option ignored (not a name-value pair): " & nvpair);
            end if;
         end;
      end loop;
            
      if not session_configuration.key_exists (get_ci_key (debug_level)) then
         declare
            name : constant String := get_ci_key (debug_level);
         begin
            case debug_level_cli is
               when silent => null;
               when high_level =>
                  session_configuration.insert (name, 1);
                  ENV.Set (Name, "1");
               when moderate =>
                  session_configuration.insert (name, 2);
                  ENV.Set (Name, "2");
               when low_level =>
                  session_configuration.insert (name, 3);
                  ENV.Set (Name, "3");               
            end case;
         end;
      end if;
      
   end set_command_line_options;
   
   
   -------------------------------
   --  set_environment_options  --
   -------------------------------
   procedure set_environment_options (session_configuration : in out ThickUCL.UclTree)
   is
      function variable_present (key : String) return Boolean;  
      procedure set_configuration (name : String; ci : Configuration_Item);
      
      UA_seen : Boolean := False;
      
      function variable_present (key : String) return Boolean is
      begin
         declare
             val : constant String := ENV.Value (key);
         begin
            return True;
         end;
      exception
         when Constraint_Error =>
            return False;
      end variable_present;
      
      procedure set_configuration (name : String; ci : Configuration_Item) 
      is
         val : constant String := ENV.Value (name);
         citype : ThickUCL.Leaf_type := get_ci_type (ci);
      begin
         case citype is
            when ThickUCL.data_not_present => null;
            when ThickUCL.data_string =>
               session_configuration.insert (name, val);
            when ThickUCL.data_integer =>
               if IsNumeric (val) then
                  session_configuration.insert (name, Ucl.ucl_integer'Value (val));                           
               else
                  EV.emit_debug (high_level, "env value of " & name & " could not " &
                                   "be converted to integer, variable unset");
                  ENV.Clear (name);
               end if;
            when ThickUCL.data_boolean =>
               if IsBoolean (val) then
                  session_configuration.insert (name, Boolean'Value (val));
               else
                  EV.emit_debug (high_level, "env value of " & name & " could not " &
                                   "be converted to boolean, variable unset");
                  ENV.Clear (name);
               end if;
            when ThickUCL.data_array =>
               EV.emit_debug (high_level, "array option " & name & " can not be set " &
                                "via the environment, variable unset");
               ENV.Clear (name);
            when ThickUCL.data_object =>  
               EV.emit_debug (high_level, "object option " & name & " can not be set " &
                                "via the environment, variable unset");
               ENV.Clear (name);
            when ThickUCL.data_time | ThickUCL.data_float =>
               EV.emit_debug (high_level, "Impossible env var " & name & " returns the " &
                                "wrong type (float, time), variable unset");
               ENV.Clear (name);
         end case;
      end set_configuration;
   begin
      for ci in Configuration_Item'Range loop
         case ci is
            when ci_not_found => null;               
            when others =>
               declare
                  name : constant String := get_ci_key (ci);                  
               begin
                  if variable_present (name) and then 
                    not session_configuration.key_exists (name) 
                  then 
                     set_configuration (name, ci);
                  end if;
               end;
         end case;
      end loop;
   end set_environment_options;
   
   -----------------------------------
   --  set_configuration_from_file  --
   -----------------------------------
   procedure set_configuration_from_file 
     (configuration_file : String;
      session_configuration : in out ThickUCL.UclTree)
   is
      procedure set_configuration (name : String; ci : Configuration_Item);
      function type2str (T : ThickUCL.Leaf_type) return String;

      temp_conf : ThickUCL.UclTree;
      file_path : constant String := Archive.Unix.real_path (configuration_file);
      
      function type2str (T : ThickUCL.Leaf_type) return String is
      begin
         case T is
            when ThickUCL.data_boolean => return "boolean";
            when ThickUCL.data_float   => return "float";
            when ThickUCL.data_integer => return "integer";
            when ThickUCL.data_time    => return "timespan";
            when ThickUCL.data_array   => return "array";
            when ThickUCL.data_object  => return "object";
            when ThickUCL.data_string  => return "string";
            when ThickUCL.data_not_present => return "NULL";
         end case;
      end type2str;
      
      procedure set_configuration (name : String; ci : Configuration_Item) 
      is
         use type ThickUCL.Leaf_type;
         citype : constant ThickUCL.Leaf_type := get_ci_type (ci);
         d_type : constant ThickUCL.Leaf_type := temp_conf.get_data_type (name);
      begin
         if d_type /= citype then
            EV.emit_notice ("Configuration file value of " & name & 
                              " ignored due to type mismatch. " & 
                              type2str (citype) & " type expected but " & 
                              type2str (d_type) & " type provided.");
            return;
         end if;
         case citype is
            when ThickUCL.data_not_present => null;               
            when ThickUCL.data_string =>
               declare
                  val : constant String := temp_conf.get_base_value (name);
               begin
                  session_configuration.insert (name, val);
                  ENV.Set (name, val);
               end;
            when ThickUCL.data_integer =>
               declare
                  val : constant Ucl.ucl_integer := temp_conf.get_base_value (name);
               begin
                  session_configuration.insert (name, val);
                  ENV.Set (name, trim (val'Img));
               end;
            when ThickUCL.data_boolean =>
               declare
                  val : constant Boolean :=  temp_conf.get_base_value (name);
               begin
                  session_configuration.insert (name, val);
                  ENV.Set (name, val'Img);
               end;
            when ThickUCL.data_array =>
               --  Rvn configuration arrays are 100% strings.  Anything else is an error
               --  Also, arrays are not pushed to the environment
               declare
                  vndx : constant ThickUCL.array_index := temp_conf.get_index_of_base_array (name);
                  array_len : constant Natural := temp_conf.get_number_of_array_elements (vndx);
                  valid : Boolean := True;
               begin
                  for x in 0 .. array_len - 1 loop
                     if temp_conf.get_array_element_type (vndx, x) /= ThickUCL.data_string then
                        valid := False;
                     end if;
                  end loop;
                  if not valid then
                     EV.emit_notice ("array of " & name & " invalid because at least one " &
                                       "element is not of type string; setting ignored");                     
                     return;
                  end if;
                  session_configuration.start_array (name);
                  for x in 0 .. array_len - 1 loop
                     declare
                        val : constant String := temp_conf.get_array_element_value (vndx, x);
                     begin
                        session_configuration.insert ("", val);
                     end;
                  end loop;
                  session_configuration.close_array;
               end;
            when ThickUCL.data_object =>
               --  Rvn configuration object payloads are 100% strings.  Anything else is an error
               --  Also, objects are not pushed to the environment
               declare
                  procedure check (Position : ThickUCL.jar_string.Cursor);
                  procedure set_nvpair (Position : ThickUCL.jar_string.Cursor);
                  
                  vndx : ThickUCL.object_index := temp_conf.get_index_of_base_ucl_object (name);
                  keys : ThickUCL.jar_string.Vector;
                  valid : Boolean := True;
                  
                  procedure check (Position : ThickUCL.jar_string.Cursor) 
                  is
                     keyname : constant String := 
                       USS (ThickUCL.jar_string.Element (Position).payload);
                  begin
                     if temp_conf.get_object_data_type (vndx, keyname) /= ThickUCL.data_string then
                        valid := False;
                     end if;
                  end check;
                  
                  procedure set_nvpair (Position : ThickUCL.jar_string.Cursor)
                  is
                     keyname : constant String := 
                       USS (ThickUCL.jar_string.Element (Position).payload);
                     value : constant String := temp_conf.get_object_value (vndx, keyname);
                  begin
                     session_configuration.insert (keyname, value);
                  end set_nvpair;
               begin
                  temp_conf.get_object_object_keys (vndx, keys);
                  keys.Iterate (check'Access);
                  if not valid then
                     EV.emit_notice ("object of " & name & " invalid because at least one " &
                                       "element is not of type string; setting ignored");                     
                     return;
                  end if;
                  session_configuration.start_object (name);
                  keys.Iterate (set_nvpair'Access);
                  session_configuration.close_object;
               end;
            when ThickUCL.data_time | ThickUCL.data_float =>
               EV.emit_debug (high_level, "Developer problem: " & name & " key has an " &
                                "unsupported type definition (" & citype'Img & ")");
         end case;
      end set_configuration;
   begin
      if IsBlank (file_path) then
         EV.emit_debug (moderate, "Given configuration file " & file_path & " does not exist.");
         return;
      end if;
      begin
         ThickUCL.Files.parse_ucl_file (temp_conf, file_path);         
      exception
         when ThickUCL.Files.ucl_file_unparseable =>
            EV.emit_notice ("Failed to parse " & file_path & "; configuration unchanged.");
            return;
      end;
      
      for ci in Configuration_Item'Range loop
         case ci is
            when ci_not_found => null;               
            when others =>
               declare
                  name : constant String := get_ci_key (ci);                  
               begin
                  if temp_conf.key_exists (name) and then
                    not session_configuration.key_exists (name) 
                  then 
                     set_configuration (name, ci);
                  end if;
               end;
         end case;
      end loop;
   end set_configuration_from_file;
   
   
   ------------------------------------------
   --  set_defaults_on_remaining_settings  --
   ------------------------------------------
   procedure set_defaults_on_remaining_settings (session_configuration : in out ThickUCL.UclTree) 
   is
      procedure set_configuration (name : String; ci : Configuration_Item);
      procedure set_configuration (name : String; ci : Configuration_Item)
      is
         citype : constant ThickUCL.Leaf_type := get_ci_type (ci);
      begin
         case citype is
            when ThickUCL.data_not_present => null;               
            when ThickUCL.data_string =>
               declare
                  val : constant String := get_default_value (ci);
               begin
                  session_configuration.insert (name, val);
                  ENV.Set (name, val);
               end;
            when ThickUCL.data_integer =>
               declare                  
                  val : constant int64 := get_default_value (ci);
               begin
                  session_configuration.insert (name, Ucl.ucl_integer (val));
                  ENV.Set (name, trim (val'Img));
               end;
            when ThickUCL.data_boolean =>
               declare
                  val : constant Boolean :=  get_default_value (ci);
               begin
                  session_configuration.insert (name, val);
                  ENV.Set (name, val'Img);
               end;
            when ThickUCL.data_array =>
               --  arrays are not pushed to the environment
               session_configuration.start_array (name);
               declare
                  procedure push (Position : string_crate.Cursor);

                  array_values : string_crate.Vector;
                  
                  procedure push (Position : string_crate.Cursor) 
                  is
                     val : constant String := USS (string_crate.Element (Position));
                  begin
                     session_configuration.insert ("", val);
                  end push;
               begin
                  get_default_value (ci, array_values);
                  array_values.Iterate (push'Access);
               end;
               session_configuration.close_array;
            when ThickUCL.data_object =>
               --  objects are not pushed to the environment
               session_configuration.start_object (name);
               declare
                  procedure push (Position : object_crate.Cursor);

                  nvpairs : object_crate.Map;
                  
                  procedure push (Position : object_crate.Cursor) 
                  is
                     key : constant String := USS (object_crate.Key (Position));
                     val : constant String := USS (object_crate.Element (Position)); 
                  begin
                     session_configuration.insert (key, val);
                  end push;
               begin
                  get_default_value (ci, nvpairs);
                  nvpairs.Iterate(push'Access);
               end;
               session_configuration.close_object;
            when ThickUCL.data_time | ThickUCL.data_float =>
               EV.emit_debug (high_level, "Developer problem: " & name & " key has an " &
                                "unsupported type definition (" & citype'Img & ")");
         end case;
      end set_configuration;
   begin
      for ci in Configuration_Item'Range loop
         case ci is
            when ci_not_found => null;               
            when others =>
               declare
                  name : constant String := get_ci_key (ci);                  
               begin
                  if not session_configuration.key_exists (name) then
                     set_configuration (name, ci);
                  end if;
               end;
         end case;
      end loop;
   end set_defaults_on_remaining_settings;
   
   
   -------------------------------
   --  establish_configuration  --
   -------------------------------
   procedure establish_configuration 
     (configuration_file    : String;
      command_line_options  : String;
      debug_level_cli       : A_Debug_Level;
      session_configuration : in out ThickUCL.UclTree)
   is
   begin
      set_command_line_options (command_line_options, debug_level_cli, session_configuration);
      set_environment_options (session_configuration);
      set_configuration_from_file (configuration_file, session_configuration);
      set_defaults_on_remaining_settings (session_configuration);
   end establish_configuration;
   
   
end Raven.Configuration;
