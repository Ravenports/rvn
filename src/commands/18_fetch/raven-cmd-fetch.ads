--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

private with Raven.Database;

package Raven.Cmd.Fetch is

   --  Executes fetch command (wrapper)
   function execute_fetch_command (comline : Cldata) return Boolean;

private

   function refresh_catalog (single_repo : String) return Boolean;

   function execute_fetch_command_core
     (rdb       : in out Database.RDB_Connection;
      repo_solo : String;
      comline   : Cldata) return Boolean;

end Raven.Cmd.Fetch;
