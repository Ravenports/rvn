--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Event;
with Raven.Context;
with Raven.Strings;
with Raven.Database.Operations;
with Raven.Database.Annotate;
with Raven.Database.Search;
with Raven.Database.Query;

use Raven.Strings;

package body Raven.Cmd.Annotate is

   package QRY renames Raven.Database.Query;
   package OPS renames Raven.Database.Operations;

   --------------------------------
   --  execute_annotate_command  --
   --------------------------------
   function execute_annotate_command (comline : Cldata) return Boolean
   is
      rdb : Database.RDB_Connection;
      unfinished_packages : Pkgtypes.Package_Set.Vector;
      behave_cs : Boolean := comline.common_options.case_sensitive;

   begin
      if Context.reveal_case_sensitive then
         behave_cs := True;
      end if;

      case OPS.rdb_open_localdb (rdb, Database.installed_packages) is
         when RESULT_OK => null;
         when others => return False;
      end case;

      Database.Search.rvn_core_search
        (db           => rdb,
         srch_pattern => USS (comline.common_options.name_pattern),
         behave_glob  => comline.cmd_info.glob_input,
         behave_exact => comline.common_options.exact_match,
         behave_cs    => behave_cs,
         s_comment    => False,
         s_desc       => False,
         s_nsv        => True,
         packages     => unfinished_packages);

      if comline.cmd_annotate.operation_find then
         display_tags (rdb, unfinished_packages, USS (comline.cmd_annotate.tag));
      elsif comline.cmd_annotate.operation_delete then
         null;
      else
         null;
      end if;

      OPS.rdb_close (rdb);
      return True;
   end execute_annotate_command;


   --------------------
   --  display_tags  --
   --------------------
   procedure display_tags (db               : Database.RDB_Connection;
                           shallow_packages : Pkgtypes.Package_Set.Vector;
                           match_tag        : String)
   is
      procedure print (Position : Pkgtypes.Package_Set.Cursor)
      is
         myrec : Pkgtypes.A_Package := Pkgtypes.Package_Set.Element (Position);

         procedure check_tag (innerpos : Pkgtypes.NV_Pairs.Cursor)
         is
            tag : constant String := USS (Pkgtypes.NV_Pairs.Key (innerpos));
         begin
            if match_tag = tag then
               declare
                  name : constant String := Pkgtypes.nsv_identifier (myrec);
                  note : constant String := USS (Pkgtypes.NV_Pairs.Element (innerpos));
                  leftname : constant String := pad_right (name, 39);
               begin
                  Event.emit_premessage (leftname & " " & tag & " => " & note);
               end;
            end if;
         end check_tag;
      begin
         QRY.finish_package_annotations (db, myrec);
         myrec.annotations.Iterate (check_tag'Access);
      end print;
   begin
      shallow_packages.Iterate (print'Access);
   end display_tags;


end Raven.Cmd.Annotate;
