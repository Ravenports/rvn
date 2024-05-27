--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

package Raven.Cmd.Fetch is

   --  Executes fetch command
   function execute_fetch_command (comline : Cldata) return Boolean;

private

   function refresh_catalog (single_repo : String) return Boolean;

end Raven.Cmd.Fetch;
