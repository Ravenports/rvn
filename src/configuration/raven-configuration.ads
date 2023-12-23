--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with ThickUCL;
private with Ada.Containers.Vectors;
private with Ada.Containers.Hashed_Maps;
private with Raven.Strings;
private with Raven.Miscellaneous;

package Raven.Configuration is

   --  The established configuration is stored in a ThickUCL tree
   --  Priority for configuration:
   --  #1 command line options
   --  #2 environment
   --  #3 configuration file
   --  Non-existent configuration file is not fatal
   procedure establish_configuration 
     (configuration_file    : String;
      command_line_options  : String;
      debug_level_cli       : A_Debug_Level;
      root_directory        : String;
      session_configuration : in out ThickUCL.UclTree);


   type Configuration_Item is
     (ci_not_found,   --  NOTFOUND
      dbdir,          --  RVN_DBDIR (/var/db/rvn)
      cachedir,       --  RVN_CACHEDIR (/var/cache/rvn)
      rc_scripts,     --  HANDLE_RC_SCRIPTS (false)   ??
      always_yes,     --  DEFAULT_ALWAYS_YES (false)
      assume_yes,     --  ASSUME_ALWAYS_YES (false)
      repos_dir,      --  REPOS_DIR ([/etc/rvn, /raven/etc/rvn/repos])
      keywords_dir,   --  KEYWORDS_DIR (/var/ravenports/conspiracy/Mk/Keywords)
      skip_dev,       --  SKIP_DEV_SUBPKG (true)
      skip_nls,       --  SKIP_NLS_SUBPKG (false)
      skip_man,       --  SKIP_MAN_SUBPKG (false)
      skip_doc,       --  SKIP_DOC_SUBPKG (false)
      skip_examples,  --  SKIP_EXAM_SUBPKG (true)
      syslog,         --  SYSLOG (true)
      abi,            --  ABI (dynamic)
      dev_mode,       --  DEVELOPER_MODE (false)
      fetch_retry,    --  FETCH_RETRY (3)
      fetch_timeout,  --  FETCH_TIMEOUT (30)
      debug_scripts,  --  DEBUG_SCRIPTS (false)
      permissive,     --  PERMISSIVE (false)
      autoupdate,     --  REPO_AUTOUPDATE (true)
      nameserver,     --  NAMESERVER ("")
      user_agent,     --  HTTP_USER_AGENT (dynamic, e.g. rvn/1.0.0)
      event_pipe,     --  EVENT_PIPE ("")
      no_timestamp,   --  UNSET_TIMESTAMP(false)
      restrict_dir,   --  SSH_RESTRICT_DIR ("")
      ssh_args,       --  SSH_ARGS ("")
      environ,        --  RVN_ENVIRON ({})
      debug_level,    --  DEBUG_LEVEL(0)
      alias,          --  ALIAS ({ object set of basic queries })
      run_scripts,    --  RUN_SCRIPTS (true)
      case_match,     --  CASE_SENSITIVE_MATCH (false)
      lock_wait,      --  LOCK_WAIT (1)
      lock_retries,   --  LOCK_RETRIES (5)
      sqlite_profile, --  SQLITE_PROFILE (false)
      read_lock,      --  READ_LOCK (false)
      automerge,      --  AUTOMERGE (true)
      create_verbose, --  CREATE_VERBOSE (false)
      autoclean,      --  AUTOCLEAN (false)
      valid_scheme,   --  VALID_URL_SCHEME([ array of schemes ])
      base_shlibs,    --  ALLOW_BASE_SHLIBS (false)
      size_limit      --  WARN_SIZE_LIMIT (1048576)
     );
      
   --  workers_count,  --  WORKERS_COUNT (0)
   --  ip_version,     --  IP_VERSION (0)  (IPv4, IPv6?)
   --  version_source  --  VERSION_SOURCE("")
   --  conservative,   --  CONSERVATIVE_UPGRADE(true)
   --  dot_file,       --  DOT_FILE ("")
   --                      OSVERSION (1302001)
   --                      IGNORE_OSVERSION (false)
   --                      BACKUP_LIBRARIES (false)
   --                      BACKUP_LIBRARY_PATH (/raven/lib/compat/rvn)
   --                      RVN_TRIGGERS_DIR (/raven/share/pkg/triggers)
   --                      RVN_TRIGGERS_ENABLE (true);
   --                      AUDIT_IGNORE_GLOB ([])
   --                      AUDIT_IGNORE_REGEX ([])
   --                      ARCHIVE_SYMLINK (false)
   --                      FILES_IGNORE_GLOB ([])
   --                      FILES_IGNORE_REGEX ([])
   
   
   --  Retrieve configuration item key given enum
   function get_ci_key (ci : Configuration_Item) return String;
   
private
   
   config_type_mismatch : exception;
   
   package string_crate is new Ada.Containers.Vectors
     (Element_Type => Text,
      Index_Type   => Natural,
      "="          => SU."=");
   
   package object_crate is new Ada.Containers.Hashed_Maps
     (Key_Type        => Text,
      Element_Type    => Text,
      Hash            => Raven.Miscellaneous.map_hash,
      Equivalent_Keys => Raven.Strings.equivalent,
      "="             => SU."=");
  
   --  Retrieve configuration item type
   function get_ci_type (ci : Configuration_Item) return ThickUCL.Leaf_type;
   
   --  Return boolean value for the given item.  A config_type_mismatch exception is
   --  thrown if that item wasn't intended to be of type boolean.
   function get_default_value (ci : Configuration_Item) return Boolean;
   
   --  Return 64-bit signed integer for the given item.  config_type_mismatch exception possible.
   function get_default_value (ci : Configuration_Item) return Int64;
   
   --  Return string value for the given item.   config_type_mismatch exception possible.
   function get_default_value (ci : Configuration_Item; root_directory : String) return String;
   
   --  Populates the given crate with a vector of strings for the given item.
   --  It represents an array of strings.  config_type_mismatch exception possible.
   procedure get_default_value (ci : Configuration_Item; crate : in out string_crate.Vector);
   
   --  Populates the given create with a associative array of strings the given item.
   --  It represents a UCL object with data types all strings.   
   --  config_type_mismatch exception possible.
   procedure get_default_value (ci : Configuration_Item; crate : in out object_crate.Map);
      
   --  Delegate the logic that sets the configuration from the command line
   --  Any valid command line options are set in the environment
   procedure set_command_line_options 
     (options               : String;
      debug_level_cli       : A_Debug_Level;
      session_configuration : in out ThickUCL.UclTree);
   
   --  Delegate the logic that sets some configuration parameters via the environment
   procedure set_environment_options 
     (session_configuration : in out ThickUCL.UclTree);
        
   --  Delegate the logic that sets some configuration parameters from a file (if it exists)
   procedure set_configuration_from_file 
     (configuration_file : String;
      session_configuration : in out ThickUCL.UclTree);
   
   --  Delegate the logic that sets the remaining configuration to the default values
   procedure set_defaults_on_remaining_settings 
     (session_configuration : in out ThickUCL.UclTree;
      root_directory        : String);
   
   --  Brute force search of the configuration item from the key
   function get_configuration_item (key : String) return Configuration_Item;
   
end Raven.Configuration;
