--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

package Raven.Cmd.Search is

   --  Executes search command
   function execute_search_command (comline : Cldata) return Boolean;

private

   function refresh_catalog (single_repo : String) return Boolean;
   function format_extra (thismod : Modifier_Data) return String;

end Raven.Cmd.Search;
