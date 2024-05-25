--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

private with Raven.Database;

package Raven.Cmd.Stats is

   --  Executes stats command
   function execute_stats_command (comline : Cldata) return Boolean;

private

   function show_catalog_stats (single_repo : String) return Boolean;
   function show_installation_stat return Boolean;

end Raven.Cmd.Stats;
