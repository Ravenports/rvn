--  SPDX-License-Identifier: ISC
--  Reference: /License.txt


with Raven.Catalog;

package body Raven.Cmd.Catalog is

   package CAT renames Raven.Catalog;


   -------------------------------
   --  execute_catalog_command  --
   -------------------------------
   function execute_catalog_command (comline : Cldata) return Boolean
   is
   begin
      --  placeholder
      return CAT.generate_database;
   end execute_catalog_command;

end Raven.Cmd.Catalog;
