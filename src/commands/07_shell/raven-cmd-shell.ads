--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

private with Interfaces.C.Strings;
private with System;

package Raven.Cmd.Shell is

   --  Executes shell command
   function execute_shell_command (comline : Cldata) return Boolean;

private

   dbpath : array (1 .. 256) of Character;

   procedure pkgshell_opendb (reponame : access system.Address);
   pragma Export (C, pkgshell_opendb);

   procedure pkgdb_init_proc;
   pragma Export (C, pkgdb_init_proc);

end Raven.Cmd.Shell;
