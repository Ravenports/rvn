--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

private with Raven.Database;

package Raven.Cmd.Shlib is

   --  Executes shlib command
   function execute_shlib_command (comline : Cldata) return Boolean;

private

   rdb : Database.RDB_Connection;

   --  Handle the --provides option
   function show_requirements (quiet : Boolean; library_soname : String) return Boolean;

   --  Handle the --requires option
   function show_provisions (quiet : Boolean; library_soname : String) return Boolean;

end Raven.Cmd.Shlib;
