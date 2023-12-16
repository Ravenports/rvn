--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Database.Operations;
with Raven.Database.CustomCmds;
with Raven.Strings;
with Raven.Event;
with sqlite_h;

use Raven.Strings;

package body Raven.Cmd.Shell is

   package OPS renames Raven.Database.Operations;
   package CMD renames Raven.Database.CustomCmds;
   package IC  renames Interfaces.C;
   package ICS renames interfaces.C.Strings;


   -----------------------------
   --  execute_shell_command  --
   -----------------------------
   function execute_shell_command (comline : Cldata) return Boolean
   is
      rdb      : Database.RDB_Connection;
      num_args : Natural := Natural (comline.cmd_shell.pass_arguments.Length);
      result   : IC.int;

      type argv_t is array (1 .. num_args) of aliased ICS.chars_ptr;
   begin
      if not OPS.localdb_exists then
         case OPS.rdb_open_localdb (rdb) is
            when RESULT_OK => OPS.rdb_close (rdb);
            when others => return False;
         end case;
      end if;

      declare
         local_database : constant String := OPS.localdb_path;
      begin
         if local_database'Length + 1 > dbpath'Length then
            Event.emit_error ("Path to sqlite database > 256 characters, prevent shell to open");
            return False;
         end if;
      end;

      declare
         argv : argv_t;
         argsval : access ICS.chars_ptr;
      begin
         for x in 1 .. num_args loop
            argv (x) := ICS.New_String (USS (comline.cmd_shell.pass_arguments.Element (x - 1)));
         end loop;

         argsval := argv (1)'Access;
         result := sqlite_h.sqlite3_shell (IC.int (num_args), argsval);

         for x in 1 .. num_args loop
            ICS.Free (argv (x));
         end loop;
      end;

      case result is
         when 0 => return True;
         when others => return False;
      end case;

   end execute_shell_command;


   -----------------------
   --  pkgdb_init_proc  --
   -----------------------
   procedure pkgdb_init_proc
   is
      init_res   : IC.int;
      unused_res : IC.int;
      pragma Unreferenced (unused_res);
   begin
      init_res := sqlite_h.sqlite3_initialize;
      case init_res is
         when sqlite_h.SQLITE_OK => null;
         when others =>
            Event.emit_error ("Failed to initialized SQLite library");
            return;
      end case;

      unused_res := sqlite_h.sqlite3_auto_extension (callback => CMD.sqlcmd_init'Access);
   end pkgdb_init_proc;


   -----------------------
   --  pkgshell_opendb  --
   -----------------------
   procedure pkgshell_opendb (reponame : access system.Address)
   is
      local_database : constant String := OPS.localdb_path;
      index : Natural := dbpath'First;
   begin
      dbpath := (others => Character'Val (0));
      for x in local_database'Range loop
         dbpath (index) := local_database (x);
         index := index + 1;
         exit when index > dbpath'Last;
      end loop;

      reponame.all := dbpath'Address;
   end pkgshell_opendb;


end Raven.Cmd.Shell;
