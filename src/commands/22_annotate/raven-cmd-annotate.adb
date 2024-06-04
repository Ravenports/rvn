--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Characters.Latin_1;
with Raven.Event;
with Raven.Context;
with Raven.Strings;
with Raven.Cmd.Unset;
with Raven.Database.Operations;
with Raven.Database.Annotate;
with Raven.Database.Search;
with Raven.Database.Query;

use Raven.Strings;

package body Raven.Cmd.Annotate is

   package LAT renames Ada.Characters.Latin_1;
   package QRY renames Raven.Database.Query;
   package OPS renames Raven.Database.Operations;
   package ANN renames Raven.Database.Annotate;
   package RCU renames Raven.Cmd.Unset;


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
         delete_tags (rdb, unfinished_packages, USS (comline.cmd_annotate.tag),
                      comline.common_options.quiet);
      elsif comline.cmd_annotate.operation_find then
         define_tags (rdb, unfinished_packages, USS (comline.cmd_annotate.tag),
                      USS (comline.cmd_annotate.note), comline.common_options.quiet);
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
                  Event.emit_message (leftname & " " & tag & " => " & note);
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


   -------------------
   --  delete_tags  --
   -------------------
   procedure delete_tags
     (db               : Database.RDB_Connection;
      shallow_packages : Pkgtypes.Package_Set.Vector;
      match_tag        : String;
      quiet            : Boolean)
   is
      deeper_packages : Pkgtypes.Package_Set.Vector;
      counter         : Natural := 0;

      procedure display_changes (Position : Pkgtypes.Package_Set.Cursor)
      is
         myrec : Pkgtypes.A_Package := Pkgtypes.Package_Set.Element (Position);
         tag_found : Boolean := False;

         procedure check_tag (innerpos : Pkgtypes.NV_Pairs.Cursor)
         is
            tag : constant String := USS (Pkgtypes.NV_Pairs.Key (innerpos));
         begin
            if match_tag = tag then
               counter := counter + 1;
               tag_found := True;
               if not quiet then
                  declare
                     name : constant String := Pkgtypes.nsv_identifier (myrec);
                     note : constant String := USS (Pkgtypes.NV_Pairs.Element (innerpos));
                     leftname : constant String := pad_right (name, 39);
                  begin
                     Event.emit_message
                       (format_removal_order (counter) & leftname & " note: " & note);
                  end;
               end if;
            end if;
         end check_tag;
      begin
         QRY.finish_package_annotations (db, myrec);
         myrec.annotations.Iterate (check_tag'Access);
         if tag_found then
            deeper_packages.Append (myrec);
         end if;
      end display_changes;
   begin
      deeper_packages.Clear;
      shallow_packages.Iterate (display_changes'Access);
      if deeper_packages.Is_Empty then
         if not quiet then
            Event.emit_message ("No packages with the '" & match_tag & "' annotation were found.");
            return;
         end if;
      end if;

      if not granted_permission_to_proceed ("removing annotations from this selection") then
         return;
      end if;

      ANN.remove_annotations (db, match_tag, deeper_packages);
      if not quiet then
         Event.emit_message ("Annotation removal complete.");
      end if;

   end delete_tags;


   ----------------------------
   --  format_removal_order  --
   ----------------------------
   function format_removal_order (counter : Natural) return String is
   begin
      if counter < 10_000 then
         return pad_left (int2str (counter), 4) & '.';
      end if;
      return pad_left (int2str (counter), 5);  --  truncates from front at 100,000+
   end format_removal_order;


   -------------------------------------
   --  granted_permission_to_proceed  --
   -------------------------------------
   function granted_permission_to_proceed (this_task : String) return Boolean
   is
      cont : Character;
   begin
      if RCU.config_setting (RCU.CFG.assume_yes) then
         return True;
      end if;

      Event.emit_message (LAT.LF & "Proceed with " & this_task & "? [y/n]: ");
      Ada.Text_IO.Get_Immediate (cont);
      case cont is
         when 'Y' | 'y' => return True;
         when others => return False;
      end case;
   end granted_permission_to_proceed;


   -------------------
   --  define_tags  --
   -------------------
   procedure define_tags
     (db               : Database.RDB_Connection;
      shallow_packages : Pkgtypes.Package_Set.Vector;
      new_tag          : String;
      new_note         : String;
      quiet            : Boolean)
   is
      counter         : Natural := 0;

      procedure display_changes (Position : Pkgtypes.Package_Set.Cursor)
      is
         myrec : Pkgtypes.A_Package := Pkgtypes.Package_Set.Element (Position);
         tag_found : Boolean := False;
         note_found : Text;

         procedure check_tag (innerpos : Pkgtypes.NV_Pairs.Cursor)
         is
            tag : constant String := USS (Pkgtypes.NV_Pairs.Key (innerpos));
         begin
            if tag = new_tag then
               tag_found := True;
               note_found := Pkgtypes.NV_Pairs.Element (innerpos);
            end if;
         end check_tag;
      begin
         counter := counter + 1;
         QRY.finish_package_annotations (db, myrec);
         myrec.annotations.Iterate (check_tag'Access);
         declare
            name : constant String := Pkgtypes.nsv_identifier (myrec);
            leftname : constant String := pad_right (name, 39);
         begin
            Event.emit_premessage (format_removal_order (counter) & leftname);
            if tag_found then
               Event.emit_message (" note: " & USS (note_found));
            else
               Event.emit_message (" no existing note found");
            end if;
         end;
      end display_changes;
   begin
      if shallow_packages.Is_Empty then
         if not quiet then
            Event.emit_error ("Nothing done since no package installation was matched.");
         end if;
         return;
      end if;

      if not quiet then
         shallow_packages.Iterate (display_changes'Access);
      end if;

      if not granted_permission_to_proceed ("setting annotations on this package set") then
         return;
      end if;

      ANN.annotate_packages (db, new_tag, new_note, shallow_packages);
      if not quiet then
         Event.emit_message ("Annotation definition complete.");
      end if;
   end define_tags;

end Raven.Cmd.Annotate;
