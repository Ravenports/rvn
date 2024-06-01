--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Event;
with Raven.Deinstall;
with Raven.Database.Remove;
with Raven.Database.Operations;
with Raven.Strings;

use Raven.Strings;

package body Raven.Cmd.Remove is

   package OPS renames Raven.Database.Operations;
   package DEL renames Raven.Database.Remove;


   ------------------------------
   --  execute_remove_command  --
   ------------------------------
   function execute_remove_command (comline : Cldata) return Boolean
   is
      success : Boolean;
      rdb     : Database.RDB_Connection;
      toplist : Pkgtypes.Package_Set.Vector;

      procedure print (Position : Pkgtypes.Package_Set.Cursor)
      is
         nsvv : constant String :=
           Pkgtypes.nsvv_identifier (Pkgtypes.Package_Set.Element (Position));
      begin
         Event.emit_message (nsvv);
      end print;
   begin
      case OPS.rdb_open_localdb (rdb, Database.installed_packages) is
         when RESULT_OK => null;
         when others => return False;
      end case;

      if comline.common_options.all_installed_pkgs then
         success := DEL.top_level_deletion_list
           (db             => rdb,
            packages       => toplist,
            pattern        => "",
            all_packages   => True,
            override_exact => comline.common_options.exact_match,
            force          => comline.cmd_remove.force_breakage);
      else
         declare
            numpatt : constant Natural := Natural (comline.cmd_remove.name_patterns.Length);
         begin
            for patt_index in 0 .. numpatt - 1 loop
               if not DEL.top_level_deletion_list
                 (db             => rdb,
                  packages       => toplist,
                  pattern        => USS (comline.cmd_remove.name_patterns.Element (patt_index)),
                  all_packages   => False,
                  override_exact => comline.common_options.exact_match,
                  force          => comline.cmd_remove.force_breakage)
               then
                  success := False;
                  exit;
               end if;
            end loop;
         end;
      end if;

      toplist.Iterate (print'Access);


      OPS.rdb_close (rdb);
      return success;
   end execute_remove_command;

end Raven.Cmd.Remove;
