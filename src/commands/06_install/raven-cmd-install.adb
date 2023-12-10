--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../../License.txt


with Raven.Event;

package body Raven.Cmd.Install is

   -------------------------------
   --  execute_install_command  --
   -------------------------------
   function execute_install_command (comline : Cldata) return Boolean
   is

   begin
      if comline.common_options.case_insensitive then
         return currently_unsupported ("--case-insensitive");
      end if;
      if comline.common_options.case_sensitive then
         return currently_unsupported ("--case-sensitive");
      end if;
      if comline.common_options.shell_glob then
         return currently_unsupported ("--glob");
      end if;
      if comline.common_options.regex then
         return currently_unsupported ("--regex");
      end if;
      if comline.common_options.no_repo_update then
         return currently_unsupported ("--no-repo-update");
      end if;
      if comline.common_options.assume_yes then
         return currently_unsupported ("--assume-yes");
      end if;
      if comline.common_options.dry_run then
         return currently_unsupported ("--dry-run");
      end if;
      if comline.common_options.quiet then
         return currently_unsupported ("--quiet");
      end if;
      if comline.cmd_install.recursive then
         return currently_unsupported ("--recursive");
      end if;
      if comline.cmd_install.automatic then
         return currently_unsupported ("--automatic");
      end if;
      if comline.cmd_install.fetch_only then
         return currently_unsupported ("--fetch-only");
      end if;
      if comline.cmd_install.force_install then
         return currently_unsupported ("--force");
      end if;
      if comline.cmd_install.inhibit_scripts then
         return currently_unsupported ("--no-scripts");
      end if;
      if comline.cmd_install.ignore_missing then
         return currently_unsupported ("--ignore-missing");
      end if;
      if comline.cmd_install.no_register then
         return currently_unsupported ("--no-registration");
      end if;
      if comline.cmd_install.only_register then
         return currently_unsupported ("--only-registration");
      end if;
      if comline.cmd_install.local_file then
         return currently_unsupported ("--file");
      end if;

      return False;
   end execute_install_command;


   -----------------------------
   --  currently_unsupported  --
   -----------------------------
   function currently_unsupported (switch : String) return Boolean is
   begin
      Raven.Event.emit_error ("switch " & switch & " is currently unsupported.");
      return False;
   end currently_unsupported;

end Raven.Cmd.Install;
