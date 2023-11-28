--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../../License.txt

with Raven.Cmd.Unset;
with Raven.Event;
with Raven.Strings; use Raven.Strings;
with Ada.Directories;
with Ada.Environment_Variables;
with Archive.Pack;
with Archive.Unix;


package body Raven.Cmd.Create is

   package RCU renames Raven.Cmd.Unset;
   package DIR renames Ada.Directories;
   package ENV renames Ada.Environment_Variables;
   
   
   ------------------------------
   --  execute_create_command  --
   ------------------------------
   function execute_create_command (comline : Cldata) return Boolean 
   is      
      function reveal_prefix return String;
      function final_rootdir return String;

      
      root_dir     : constant String := USS (comline.cmd_create.rootdir_dir);
      output_dir   : constant String := USS (comline.cmd_create.output_dir);
      metadata     : constant String := USS (comline.cmd_create.metadata_file);
      whitelist    : constant String := USS (comline.cmd_create.whitelist_file);
      basename     : constant String := USS (comline.common_options.name_pattern);
      rvn_filename : constant String := rvn_file (basename, creation_directory (output_dir));
      keywords_dir : constant String := RCU.config_setting (RCU.CFG.keywords_dir);
      verbosity    : constant Boolean := RCU.config_setting (RCU.CFG.create_verbose);
      timestamp    : constant Archive.filetime := provide_timestamp (comline.cmd_create.timestamp);
      level        : Archive.info_level := Archive.silent;
      
      --------------------
      -- reveal_prefix  --
      --------------------
      function reveal_prefix return String 
      is
         given : constant String := USS (comline.cmd_create.prefix);
      begin
         if given = "" then
            return "/";
         end if;
         return given;
      end reveal_prefix;
      
      ---------------------
      --  final_rootdir  --
      ---------------------
      function final_rootdir return String is
      begin
         if isBlank (root_dir) then
            return "/";
         end if;
         return root_dir;
      end final_rootdir;
      
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
      end if;
      
      if not Archive.Pack.integrate (top_level_directory => final_rootdir,
                                     metadata_file       => metadata,
                                     manifest_file       => whitelist,
                                     prefix              => reveal_prefix,
                                     keyword_dir         => keywords_dir,
                                     fixed_timestamp     => timestamp,
                                     output_file         => rvn_filename,
                                     verbosity           => level)
      then
         return False;
      end if;
            
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
      extension : constant String := ".rvn";
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
