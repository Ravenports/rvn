--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Cmd.Unset;
with Raven.Event;
with Raven.Metadata;
with Raven.Context;
with Raven.Strings; use Raven.Strings;
with Ada.Directories;
with Ada.Environment_Variables;
with Archive.Pack;
with Archive.Unix;
with ThickUCL.Files;


package body Raven.Cmd.Create is

   package RCU renames Raven.Cmd.Unset;
   package MET renames Raven.Metadata;
   package DIR renames Ada.Directories;
   package ENV renames Ada.Environment_Variables;


   ------------------------------
   --  execute_create_command  --
   ------------------------------
   function execute_create_command (comline : Cldata) return Boolean
   is
      function reveal_prefix return String;
      function final_rootdir return String;
      function determine_basename return String;

      root_dir     : constant String := USS (comline.cmd_create.rootdir_dir);
      output_dir   : constant String := USS (comline.cmd_create.output_dir);
      metadata     : constant String := USS (comline.cmd_create.metadata_file);
      whitelist    : constant String := USS (comline.cmd_create.whitelist_file);
      package_abi  : constant String := RCU.config_setting (RCU.CFG.abi);
      keywords_dir : constant String := RCU.config_setting (RCU.CFG.keywords_dir);
      verbosity    : constant Boolean := RCU.config_setting (RCU.CFG.create_verbose);
      rec_baselib  : constant Boolean := RCU.config_setting (RCU.CFG.base_shlibs);
      timestamp    : constant Archive.filetime := provide_timestamp (comline.cmd_create.timestamp);
      level        : Archive.info_level := Archive.normal;
      remote_FD    : Archive.Unix.File_Descriptor;
      no_pkgname   : Boolean := False;
      meta_parsed  : Boolean := False;
      metatree     : ThickUCL.UclTree;

      --------------------
      -- reveal_prefix  --
      --------------------
      function reveal_prefix return String
      is
         --  A prefix defined in metadata has priority
         cmd_prefix : constant String := USS (comline.cmd_create.prefix);
      begin
         if IsBlank (metadata) then
            return cmd_prefix;
         end if;
         if not meta_parsed then
            begin
               ThickUCL.Files.parse_ucl_file (metatree, metadata, "");
               meta_parsed := True;
            exception
               when ThickUCL.Files.ucl_file_unparseable =>
               Raven.Event.emit_error ("prefix determination: metadata file failed to parse");
            end;
         end if;
         if meta_parsed then
            if MET.string_data_exists (metatree, MET.prefix) then
               if not isBlank (cmd_prefix) then
                  Raven.Event.emit_notice
                    ("Prefix found in metadata has priority; value of --prefix ignored.");
               end if;
               return MET.get_string_data (metatree, MET.prefix);
            end if;
         end if;
         return cmd_prefix;
      end reveal_prefix;

      ---------------------
      --  final_rootdir  --
      ---------------------
      function final_rootdir return String is
      begin
         if isBlank (root_dir) then
            return "/";
         end if;
         return Archive.Unix.real_path (root_dir);
      end final_rootdir;

      ------------------------
      -- determine_basename --
      ------------------------
      function determine_basename return String
      is
         --  must be run after it's determined metadata file is valid (if provided)
         hyphen : constant Character := '-';
      begin
         if not IsBlank (comline.common_options.name_pattern) then
            return USS (comline.common_options.name_pattern);
         end if;
         --  No pkg-name given, we need to assemble one
         if IsBlank (metadata) then
            no_pkgname := True;
            Raven.Event.emit_error ("metadata is not optional if pkg-name is not provided.");
            return "";
         end if;
         Raven.Event.emit_debug (moderate, "Need to assemble package name from metadata");
         begin
            ThickUCL.Files.parse_ucl_file (metatree, metadata, "");
            meta_parsed := True;
            if MET.string_data_exists (metatree, MET.namebase) and then
              MET.string_data_exists (metatree, MET.subpackage) and then
              MET.string_data_exists (metatree, MET.variant) and then
              MET.string_data_exists (metatree, MET.version)
            then
               return
                 MET.reveal_namebase (metatree) & hyphen &
                 MET.reveal_subpackage (metatree) & hyphen &
                 MET.reveal_variant (metatree) & hyphen &
                 MET.reveal_version (metatree);
            else
               Raven.Event.emit_error ("pkg-name was not provided, but metadata " &
                                         "is missing name string components to determine it.");
            end if;
         exception
            when ThickUCL.Files.ucl_file_unparseable =>
               Raven.Event.emit_error ("pkg-name determination: metadata file failed to parse");
         end;
         no_pkgname := True;
         return "";
      end determine_basename;
   begin
      if not valid_directory (root_dir, "root") then
         return False;
      end if;

      if not valid_directory (output_dir, "output") then
         return False;
      end if;

      if not valid_directory (keywords_dir, "keywords") then
         return False;
      end if;

      if not valid_file (whitelist, "whitelist") then
         return False;
      end if;

      if not valid_file (metadata, "metadata") then
         return False;
      end if;

      if comline.common_options.verbose or else verbosity then
         level := Archive.verbose;
      elsif comline.common_options.quiet then
         level := Archive.silent;
      end if;
      case comline.pre_command.debug_setting is
         when low_level => level := Archive.debug;
         when others => null;
      end case;
      remote_FD := Archive.Unix.File_Descriptor (Context.reveal_event_pipe);

      declare
         basename     : constant String := determine_basename;
         rvn_filename : constant String := rvn_file (basename, creation_directory (output_dir));
      begin
         if no_pkgname then
            return False;
         end if;

         if not Archive.Pack.integrate (top_level_directory => final_rootdir,
                                        metadata_file       => metadata,
                                        manifest_file       => whitelist,
                                        prefix              => reveal_prefix,
                                        abi                 => package_abi,
                                        keyword_dir         => keywords_dir,
                                        fixed_timestamp     => timestamp,
                                        output_file         => rvn_filename,
                                        verbosity           => level,
                                        record_base_libs    => rec_baselib,
                                        optional_pipe       => remote_FD)
         then
            return False;
         end if;
      end;

      return True;
   end execute_create_command;


   --------------------------
   --  creation_directory  --
   --------------------------
   function creation_directory (outdir : String)  return String is
   begin
      if IsBlank (outdir) then
         return ".";
      end if;
      return outdir;
   end creation_directory;


   -------------------------
   --  provide_timestamp  --
   -------------------------
   function provide_timestamp (arg : text) return Archive.filetime
   is
      --  Priority #1: command line setting
      --  Priority #2: SOURCE_DATE_EPOCH in the environment
      result : Archive.filetime := 0;
      SDEKEY : constant String := "SOURCE_DATE_EPOCH";
   begin
      if SU.Length (arg) > 0 then
         declare
            argstr : constant String := USS (arg);
            tmpres : Archive.filetime;
         begin
            tmpres := Archive.filetime'Value (argstr);
            result := tmpres;
         exception
            when Constraint_Error =>
               Raven.Event.emit_error ("Dev error (provide_timestamp), should be impossible");
         end;
      else
         if ENV.Exists (SDEKEY) then
            declare
               val : constant String := ENV.Value (SDEKEY);
               tmpres : Archive.filetime;
            begin
               tmpres := Archive.filetime'Value (val);
               result := tmpres;
            exception
               when Constraint_Error =>
                  Raven.Event.emit_message ("SOURCE_DATE_EPOCH is not numeric, ignored.");
            end;
         end if;
      end if;
      return result;
   end provide_timestamp;


   ----------------
   --  rvn_file  --
   ----------------
   function rvn_file (filename : String; creation_dir : String) return String
   is
      fname     : constant String := tail (filename, "/");
      out_level : constant String := Archive.Unix.real_path (creation_dir);
   begin
      if fname'Length > extension'Length and then
        lowercase (fname (fname'Last - extension'Length + 1 .. fname'Last)) = extension
      then
         return out_level & "/" & fname;
      end if;
      return out_level & "/" & fname & extension;
   end rvn_file;


   ------------------------
   --  valid_directory  --
   -----------------------
   function valid_directory (checkdir : String; description : String) return Boolean
   is
      opt_directory : constant Boolean := not IsBlank (checkdir);
   begin
      if not opt_directory then
         return True;
      end if;

      --  verify directory exists and is a directory.
      if DIR.Exists (checkdir) then
         case DIR.Kind (checkdir) is
            when DIR.Directory =>
               return True;
            when others =>
               Raven.Event.emit_error ("create:" & description & " argument (" & checkdir
                                       & ") is not a directory.");
               return False;
         end case;
      end if;
      Raven.Event.emit_error ("create:"  & description & " directory (" & checkdir
                              & ") does not exist.");
      return False;
   end valid_directory;


   ------------------
   --  valid_file  --
   ------------------
   function valid_file
     (checkfile   : String;
      description : String) return Boolean
   is
      opt_file : constant Boolean := not IsBlank (checkfile);
   begin
      if not opt_file then
         return True;
      end if;

      --  verify file exists and is a regular file.
      if DIR.Exists (checkfile) then
         case DIR.Kind (checkfile) is
            when DIR.Ordinary_File =>
               return True;
            when others =>
               Raven.Event.emit_error ("create:" & description & " argument (" & checkfile
                                       & ") is not a file.");
               return False;
         end case;
      end if;

      Raven.Event.emit_error ("create:" & description & " file (" & checkfile &
                                ") does not exist.");
      return False;
   end valid_file;

end Raven.Cmd.Create;
