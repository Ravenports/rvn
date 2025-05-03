--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

private with Raven.Database;

package Raven.Cmd.Stats is

   --  Executes stats command
   function execute_stats_command (comline : Cldata) return Boolean;

private

   function show_catalog_stats (single_repo : String; option_quiet : Boolean) return Boolean;
   function show_installation_stats return Boolean;
   procedure show_cache_stats;
end Raven.Cmd.Stats;
