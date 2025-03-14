--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Characters.Latin_1;
with Ada.Directories;
with Ada.Command_Line;
with GNAT.OS_Lib;
with Archive.Unix;

with Raven.Strings; use Raven.Strings;

package body Raven.Cmd.Help is

   package LAT renames Ada.Characters.Latin_1;
   package CLI renames Ada.Command_Line;
   package DIR renames Ada.Directories;
   package OSL renames GNAT.OS_Lib;

   ----------------------------
   --  execute_help_command  --
   ----------------------------
   function execute_help_command (comline : Cldata) return Boolean
   is
   begin
      --  Check for non-command help pages first, otherwise they never show.
      case comline.help_command2 is
         when cv2_unset => null;
         when cv2_main =>
            return show_man_page (progname, '8');
         when cv2_main_conf =>
            return show_man_page (progname & ".conf", '5');
         when cv2_repository =>
            return show_man_page (progname & "-repository", '5');
         when cv2_keywords =>
            return show_man_page (progname & "-keywords", '5');
         when cv2_scripts =>
            return show_man_page (progname & "-scripts", '5');
         when cv2_luascripts =>
            return show_man_page (progname & "-lua-scripts", '5');
         when cv2_ravensign =>
            return show_man_page ("ravensign", '7');
         when cv2_query_format =>
            return show_man_page (progname & "-query-format", '7');
      end case;


      --  There's no man page rvn-help.8.  Trying "rvn help help" will result
      --  in a manpage not found error.
      if comline.help_command = cv_unset or else
        comline.help_command = cv_help
      then
         print_global_options;
         print_command_summary;
         TIO.Put_Line ("");
         TIO.Put_Line ("For more information on the different commands see "
                       & SQ (progname & " help <command>") & ".");
         return True;
      end if;

      return show_man_page
        (progname & "-" & convert_command_enum_to_label (comline.help_command), '8');

   end execute_help_command;


   ----------------------------
   --  print_global_options  --
   ----------------------------
   procedure print_global_options
   is
      prog : constant String := progname & "(8) ";
   begin
      TIO.Put_Line ("Global options supported:");
      PL ("-d", "Increment debug level");
      PL ("-c", "Execute " & prog & "inside a chroot(8)");
      PL ("-C", "Use the specified configuration file");
      PL ("-r", "Relocate installation from / to <rootdir>");
      PL ("-R", "Directory to search for individual repository configurations");
      PL ("-o", "Override configuration option from the command line");
      PL ("-l", "List available commands and exit");
      PL ("-4", "Only use IPv4");
      PL ("-6", "Only use IPv6");
      PL ("-v", "Display " & prog & "version");
      PL ("-v -v", "Display all configuration settings");
      PL ("--status-check", "Check " & prog & " functionality non-invasively");
   end print_global_options;


   -----------------------------
   --  print_command_summary  --
   -----------------------------
   procedure print_command_summary is
   begin
      TIO.Put_Line ("");
      TIO.Put_Line ("Commands supported:");
      for command in Command_verb'Range loop
         declare
            C : constant String := convert_command_enum_to_label (command);
         begin
            case command is
               when cv_unset   => null;
               when cv_alias   => PL (C, "Lists the command line aliases");
               when cv_audit   => PL (C, "Lists active cybersecurity vulnerabilities");
               when cv_autoremove => PL (C, "Removes orphan packages");
               when cv_annotate   => PL (C, "Manipulate installed package annotations");
               when cv_catalog => PL (C, "Ensures local package catalog is up to date");
               when cv_check   => PL (C, "verifies integrity of installed packages");
               when cv_clean   => PL (C, "cleans the local cache of downloaded packages");
               when cv_config  => PL (C, "Displays the value of a configuration option");
               when cv_create  => PL (C, "Creates software packages for distribution");
               when cv_fetch   => PL (C, "Fetches packages from a remote repository");
               when cv_genrepo => PL (C, "Creates a package repository catalog");
               when cv_help    => PL (C, "Displays help information");
               when cv_info    => PL (C, "Displays information about installed packages");
               when cv_install => PL (C, "Installs packages from remote and local repositories");
               when cv_query   => PL (C, "Queries installed packages database");
               when cv_remove  => PL (C, "Removes packages from the database and the system");
               when cv_rquery  => PL (C, "Queries remote catalog");
               when cv_search  => PL (C, "Performs search of the package repository catalog");
               when cv_shell   => PL (C, "Opens a command shell for the local SQLite database");
               when cv_shlib   => PL (C, "Displays packages that link against a specific library");
               when cv_stats   => PL (C, "Displays package database statistics");
               when cv_upgrade => PL (C, "Upgrades installed packages");
               when cv_version => PL (C, "Displays the currency of installed packages");
               when cv_which   => PL (C, "Displays which package installed a specific file");
            end case;
         end;
      end loop;
   end print_command_summary;


   ----------
   --  PL  --
   ----------
   procedure PL (name, value : String)
   is
      width     : constant Natural := 14;
      namespace : String (1 .. width + 1) := (others => ' ');
      namelen   : Natural := name'Length;
   begin
      if namelen > width then
         namelen := width;
      end if;
      namespace (1 .. namelen) := name (name'First .. name'First + namelen - 1);
      TIO.Put_Line (LAT.HT & namespace & value);
   end PL;


   ---------------------
   --  show_man_page  --
   ---------------------
   function show_man_page (manpage : String; section : Character) return Boolean
   is
      mandoc     : constant String := install_loc & "/bin/man";
      sysman     : constant String := "/usr/bin/man";
      use_mandoc : Boolean := False;

      function manprog return String is
      begin
         if use_mandoc then
            return mandoc;
         end if;
         return sysman;
      end manprog;

      function manpage_location return String
      is
         --  search priority
         --  realpath (../share/man/man<section>/<manpage>)
         --  If command doesn't resolve with realpath, use install location

         postman : constant String := section & "/" & manpage & "." & section;
         cmd_fullpath : constant String := Archive.Unix.real_path (CLI.Command_Name);
      begin
         if IsBlank (cmd_fullpath) then
            return install_loc & "/share/man/man" & postman;
         end if;

         declare
            zero : constant String := head (cmd_fullpath, "/") & "/../share/man/man";
            first_choice : constant String := Archive.Unix.real_path (zero & postman);
         begin
            if isBlank (first_choice) then
               return Archive.Unix.real_path (zero & postman);
            end if;
            return first_choice;
         end;
      end manpage_location;

   begin
      if DIR.Exists (mandoc) then
         use_mandoc := True;
      elsif not DIR.Exists (sysman) then
         TIO.Put_Line (TIO.Standard_Error, progname & ": No man program found");
         return False;
      end if;
      if IsBlank (manpage_location) then
         TIO.Put_Line (TIO.Standard_Error, progname & ": missing manpage: " & manpage
                       & "(" & section & ")");
         return False;
      end if;

      declare
         Result    : Integer;
         Arguments : OSL.Argument_List := (1 => new String'(manpage_location));
      begin
         OSL.Spawn (Program_Name           => manprog,
                    Args                   => Arguments,
                    Output_File_Descriptor => OSL.Standout,
                    Return_Code            => Result,
                    Err_To_Out             => True);
         for Index in Arguments'Range loop
            OSL.Free (Arguments (Index)); -- Free the argument list
         end loop;
         return (Result = 0);
      end;

   end show_man_page;

end Raven.Cmd.Help;
