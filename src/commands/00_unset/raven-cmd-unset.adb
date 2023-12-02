--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Environment_Variables;
with Raven.Context;
with Raven.Event;
with Raven.Unix;
with Raven.Strings; use Raven.Strings;
with ThickUCL.Emitter;
with Ucl;

package body Raven.Cmd.Unset is
   
   package ENV renames Ada.Environment_Variables;
   package EV  renames Raven.Event;
   
   
   --------------------------
   --  execute_no_command  --
   --------------------------
   function execute_no_command (comline : Cldata) return Boolean is
   begin
      case comline.pre_command.version_setting is
         when not_shown =>
            --  switch --list
            if comline.pre_command.list_commands then   
               return list_available_commands;
            end if;
            if comline.pre_command.status_check then
               initialize_program (comline);
               return do_status_check;
            end if;
            return False;
         when just_version =>
            --  switch -v
            return basic_version_info;          
         when dump_configuration =>
            --  switch -vv
            initialize_program (comline);
            return extended_version_info;
      end case;
   end execute_no_command;
   

   --------------------------
   --  basic_version_info  --
   --------------------------
   function basic_version_info return Boolean is
   begin
      TIO.Put_Line (progversion);
      return True;
   end basic_version_info;
   
   
   -----------------------
   --  do_status_check  --
   -----------------------
   function do_status_check return Boolean is
   begin
      TIO.Put_Line ("status-check not yet implemented");
      return False;
   end do_status_check;
   
   
   ------------------------------
   --  list_available_comands  --
   ------------------------------
   function list_available_commands return Boolean
   is
      -- start with support for 15 commands (not including unset)
      -- As this is exceeded add an additional row of 5 columns
      type rows is range 1 .. 3;
      type cols is range 1 .. 5;
      type A_column is array (rows) of Command_verb;
      matrix : array (cols) of A_column;
      index : Natural := 0;
   begin
      for x in cols loop
         for y in rows loop
            matrix (x)(y) := cv_unset;
         end loop;
      end loop;
      declare
         row : rows := 1;
         col : cols := 1;
      begin
         for cv in Command_verb'Range loop
            case cv is
               when cv_unset => null;
               when others =>
                  matrix (col)(row) := cv;
                  if row = rows'Last then
                     row := rows'First;
                     col := col + 1;
                  else
                     row := row + 1;
                  end if;
            end case;
         end loop;
      end;
      
      for y in rows loop
         for x in cols loop
            declare
               c : constant String := convert_command_enum_to_label (matrix (x)(y));
            begin
               TIO.Put (pad_right (C, 15));
            end;
         end loop;
         TIO.Put_Line ("");
      end loop;
      return True;
   end list_available_commands;
   
   
   --------------------------
   --  initialize_program  --
   --------------------------
   procedure initialize_program (comline : Cldata) 
   is
      function config_file_path return String;
      
      --  establish configuration
      --  set environment 
      --  set context
      --  special debug handling (if specified in command line:
      --          * set context early
      --          * passes to establish_configuration
      --          * reset context
      
      function config_file_path return String 
      is
         default_location : constant String := install_loc & "/etc/" & progname & ".conf";
      begin
         if IsBlank (comline.pre_command.custom_configfile) then
            return default_location;
         end if;
         return USS (comline.pre_command.custom_configfile);
      end config_file_path;
   begin
      Context.register_debug_level (comline.pre_command.debug_setting);
      CFG.establish_configuration 
        (configuration_file    => config_file_path, 
         command_line_options  => USS (comline.pre_command.option_nvpairs), 
         debug_level_cli       => comline.pre_command.debug_setting, 
         session_configuration => program_configuration);
      
      declare
         key1 : constant String := CFG.get_ci_key (CFG.debug_level);
         key2 : constant String := CFG.get_ci_key (CFG.dbdir);
         key3 : constant String := CFG.get_ci_key (CFG.cachedir);
         key4 : constant String := CFG.get_ci_key (CFG.event_pipe);
         key5 : constant String := CFG.get_ci_key (CFG.dev_mode);
         
         conf_debug : constant Ucl.ucl_integer := program_configuration.get_base_value (key1);
         db_dir     : constant String := program_configuration.get_base_value (key2); 
         cache_dir  : constant String := program_configuration.get_base_value (key3);
         event_pipe : constant String := program_configuration.get_base_value (key4);
         dev_mode   : constant Boolean := program_configuration.get_base_value (key5);   
         
         mechanism  : Unix.Unix_Pipe;
      begin
         case conf_debug is
            when 0 => Context.register_debug_level (silent);
            when 1 => Context.register_debug_level (high_level);
            when 2 => Context.register_debug_level (moderate);
            when 3 => Context.register_debug_level (low_level);
            when others => null;
         end case;
         Context.register_db_directory (db_dir);
         Context.register_cache_directory (cache_dir);
         Context.register_dev_mode (dev_mode);
         if not isBlank (event_pipe) then
            mechanism := Unix.IPC_mechanism (event_pipe);
            case mechanism is
               when Unix.named_pipe =>
                  if Context.register_event_pipe_via_file (event_pipe) then
                     EV.emit_debug (moderate, "FIFO Event pipe " & SQ (event_pipe) & " opened.");
                  else
                     EV.emit_errno ("open event pipe (FIFO)", event_pipe, Unix.errno);
                  end if;
               when Unix.unix_socket =>
                  case Context.register_event_pipe_via_socket (event_pipe) is
                     when Unix.connected =>
                        EV.emit_debug 
                          (moderate, "Event pipe " & SQ (event_pipe) & " socket opened.");
                     when Unix.failed_creation
                        | Unix.failed_connection =>
                        EV.emit_errno ( "open event pipe (socket)", event_pipe, Unix.errno);
                     when Unix.failed_population =>
                        EV.emit_error ("Socket path too long: " & event_pipe);
                  end case;
               when Unix.something_else =>
                  EV.emit_error (event_pipe & " is not a fifo or socket");
            end case;
         end if;
      end;
      
      declare
         procedure upsert (Position : ThickUCL.jar_string.Cursor);
         
         nkey : constant String := CFG.get_ci_key (CFG.environ);
         keys : ThickUCL.jar_string.Vector;
         vndx : ThickUCL.object_index := program_configuration.get_index_of_base_ucl_object (nkey);
         
         procedure upsert (Position : ThickUCL.jar_string.Cursor) 
         is
            name : constant String := USS (ThickUCL.jar_string.Element (Position).payload);
            val  : constant String := program_configuration.get_object_value (vndx, name);
         begin
            ENV.Set (name, val);
         end upsert;
      begin
         program_configuration.get_object_object_keys (vndx, keys); 
         keys.Iterate (upsert'Access);
      end;
   end initialize_program;
   
   
   -----------------------------
   --  extended_version_info  --
   -----------------------------
   function extended_version_info return Boolean is
   begin
      TIO.Put_Line ("Version: " & progversion);
      TIO.Put_Line (ThickUCL.Emitter.emit_ucl (program_configuration));
      show_repository_info;

      return True;
   end extended_version_info;
   
   
   ----------------------------
   --  show_repository_info  --
   ----------------------------
   procedure show_repository_info is
   begin
      --  TODO: Re-implement
      null;
   end show_repository_info;
   
   
   ------------------------
   --  alias_definition  --
   ------------------------
   function alias_definition (alias : String) return String 
   is
      key : constant String := CFG.get_ci_key (CFG.alias);
      vndx : ThickUCL.object_index;
      dtype : ThickUCL.Leaf_type;
   begin
      if program_configuration.key_exists (key) then
         vndx := program_configuration.get_index_of_base_ucl_object (key);
         dtype := program_configuration.get_object_data_type (vndx, alias);
         case dtype is
            when ThickUCL.data_string =>
               return program_configuration.get_object_value (vndx, alias);
            when others =>
               null;
         end case;
      end if;
      return "";
   end alias_definition;
   
   
   -------------------------
   --  config_setting #1  --
   -------------------------
   function config_setting (setting : CFG.Configuration_Item) return String 
   is
      key   : constant String := CFG.get_ci_key (setting);
      dtype : ThickUCL.Leaf_type;
   begin
      dtype := program_configuration.get_data_type (key);
      case dtype is
         when ThickUCL.data_string =>
            return program_configuration.get_base_value (key);
         when others =>
            raise ThickUCL.ucl_type_mismatch with key & " is not of type string";  
      end case;
   end config_setting;
   
   
   -------------------------
   --  config_setting #2  --
   -------------------------
   function config_setting (setting : CFG.Configuration_Item) return Boolean
   is
      key   : constant String := CFG.get_ci_key (setting);
      dtype : ThickUCL.Leaf_type;
   begin
      dtype := program_configuration.get_data_type (key);
      case dtype is
         when ThickUCL.data_boolean =>
            return program_configuration.get_base_value (key);
         when others =>
            raise ThickUCL.ucl_type_mismatch with key & " is not of type boolean";  
      end case;
   end config_setting;
   
   
   -------------------------
   --  config_setting #3  --
   -------------------------
   function config_setting (setting : CFG.Configuration_Item) return int64 
   is
      key   : constant String := CFG.get_ci_key (setting);
      dtype : ThickUCL.Leaf_type;
      value : Ucl.ucl_integer;
   begin
      dtype := program_configuration.get_data_type (key);
      case dtype is
         when ThickUCL.data_integer =>
            value := program_configuration.get_base_value (key);
            return int64 (value);
         when others =>
            raise ThickUCL.ucl_type_mismatch with key & " is not of type int64";  
      end case;
   end config_setting;
   
   --------------------------------
   --  config_setting_as_string  --
   --------------------------------
   function config_setting_as_string (setting : CFG.Configuration_Item) return String 
   is
      key   : constant String := CFG.get_ci_key (setting);
      dtype : ThickUCL.Leaf_type;
   begin
      dtype := program_configuration.get_data_type (key);
      case dtype is
         when ThickUCL.data_string => 
            return config_setting (setting);
         when ThickUCL.data_boolean =>
            declare
               val : boolean;
            begin
               val := config_setting (setting);
               return val'Img;
            end;
         when ThickUCL.data_integer =>
            declare
               val : int64;
            begin
               val := config_setting (setting);
               return trim (val'Img);
            end;
         when ThickUCL.data_not_present =>
            return "DEVERR_data_not_present";
         when ThickUCL.data_time =>
            return "DEVERR_time_not_supported";
         when ThickUCL.data_float =>
            return "DEVERR_float_not_supported";
         when ThickUCL.data_array =>
            declare
               key    : constant String := CFG.get_ci_key (setting);
               vndx   : ThickUCL.array_index;
               restxt : Text;
               delim  : Character := Character'Val (0);
               num_elements : Natural;
            begin
               vndx := program_configuration.get_index_of_base_array (key);
               num_elements := program_configuration.get_number_of_array_elements (vndx);
               for index in 0 .. num_elements - 1 loop
                  declare
                     val : String := program_configuration.get_array_element_value (vndx, index);
                  begin
                     if index = 0 then
                        restxt := SUS (val);
                     else
                        SU.Append (restxt, delim & val);
                     end if;
                  end;
               end loop;
               return USS (restxt);
            end;
         when ThickUCL.data_object =>
            declare
               procedure scan (Position : ThickUCL.jar_string.Cursor); 
                 
               key    : constant String := CFG.get_ci_key (setting);
               vndx   : ThickUCL.object_index;
               delim  : Character := Character'Val (0);
               okeys  : ThickUCL.jar_string.Vector;
               restxt : Text;
               
               procedure scan (Position : ThickUCL.jar_string.Cursor) 
               is
                  dkey : constant String := USS (ThickUCL.jar_string.Element (Position).payload);
                  val  : constant String := program_configuration.get_object_value (vndx, dkey);
               begin
                  if IsBlank (restxt) then
                     restxt := SUS (dkey & ": " & val);
                  else
                     SU.Append (restxt, delim & dkey & ": " & val);
                  end if;
               end scan;
            begin
               vndx := program_configuration.get_index_of_base_ucl_object (key);
               program_configuration.get_object_object_keys (vndx, okeys);
               okeys.Iterate (scan'Access);
               return USS (restxt);
            end;
      end case;
   end config_setting_as_string;
   
   --------------------------------
   --  config_setting_map_value  --
   --------------------------------
   function config_setting_map_value
     (setting : CFG.Configuration_Item;
      map_key : String;
      success : in out Boolean) return String
   is
      key   : constant String := CFG.get_ci_key (setting);
      dtype : ThickUCL.Leaf_type;
   begin
      success := False;
      dtype := program_configuration.get_data_type (key);
      case dtype is
         when ThickUCL.data_object =>
            declare
               key   : constant String := CFG.get_ci_key (setting);
               vndx  : ThickUCL.object_index;
               mtype : ThickUCL.Leaf_type;
            begin
               vndx := program_configuration.get_index_of_base_ucl_object (key);
               mtype := program_configuration.get_object_data_type (vndx, map_key);
               case mtype is
                  when ThickUCL.data_not_present =>
                     return progname & ": No such alias: '" & map_key & "'";
                  when ThickUCL.data_string =>
                     success := True;
                     return map_key & ": " & program_configuration.get_object_value (vndx, map_key);
                  when ThickUCL.data_boolean =>
                     success := True;
                     declare
                        val : Boolean;
                     begin
                        val := program_configuration.get_object_value (vndx, map_key);
                        return map_key & ": " & val'Img;
                     end;
                  when ThickUCL.data_integer =>
                     success := True;
                     declare
                        val : Ucl.ucl_integer;
                     begin
                        val := program_configuration.get_object_value (vndx, map_key);
                        return map_key & ": " & trim (Int64 (val)'Img);
                     end;
                  when ThickUCL.data_time =>
                     return "DEVERR2_time_not_supported";
                  when ThickUCL.data_float =>
                     return "DEVERR2_float_not_supported";
                  when ThickUCL.data_array =>
                     return "DEVERR2_array_not_supported";
                  when ThickUCL.data_object =>
                     return "DEVERR2_object_not_supported";
               end case;
            end;
         when others =>
            raise ThickUCL.ucl_type_mismatch with key & " is not of type object";  
      end case;
   end config_setting_map_value;
   
   
   -------------------------------
   --  exit_status_already_set  --
   -------------------------------
   function exit_status_already_set return Boolean is
   begin
      return sys_exit_overridden;
   end exit_status_already_set;
   
   
   ----------------------------
   --  override_exit_status  --
   ----------------------------
   procedure override_exit_status is
   begin
      sys_exit_overridden := True;
   end override_exit_status;
   
end Raven.Cmd.Unset;
