--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Command_Line;
with Ada.Characters.Latin_1;
with Archive.Unix;
with Archive.Unpack;
with ThickUCL.Files;
with ThickUCL.Emitter;
with Raven.Cmd.Unset;
with Raven.Event;
with Raven.Context;
with Raven.Strings;
with Raven.Database.Lock;
with Raven.Database.Query;
with Raven.Database.Search;
with Raven.Database.Operations;

Use Raven.Strings;

package body Raven.Cmd.Info is

   package RCU renames Raven.Cmd.Unset;
   package LOK renames Raven.Database.Lock;
   package OPS renames Raven.Database.Operations;
   package LAT renames Ada.Characters.Latin_1;


   -----------------------------
   --  Executes info command  --
   -----------------------------
   function execute_info_command (comline : Cldata) return Boolean
   is
      procedure increment (attribute : Boolean);

      num_attr_selected : Natural := 0;
      cmd : switches_info_cmd renames comline.cmd_info;

      procedure increment (attribute : Boolean) is
      begin
         if attribute then
            num_attr_selected := num_attr_selected + 1;
         end if;
      end increment;

   begin
      --  skip exists, raw, path-archive-file  (raw with attribute is an error)
      increment (cmd.annotations);
      increment (cmd.comment);
      increment (cmd.dependencies);
      increment (cmd.description);
      increment (cmd.install_message);
      increment (cmd.install_prefix);
      increment (cmd.list_files);
      increment (cmd.list_digests);
      increment (cmd.list_attributes);
      increment (cmd.namebase);
      increment (cmd.rev_deps);
      increment (cmd.shlibs_adjacent);
      increment (cmd.shlibs_provided);
      increment (cmd.shlibs_used);
      increment (cmd.subpackage);
      increment (cmd.total_size);
      increment (cmd.variant);
      increment (cmd.abi);
      increment (cmd.options);

      if IsBlank (comline.cmd_info.path_archive_file) then
         return execute_installed_info_command (comline, num_attr_selected);
      end if;

      if cmd.raw_manifest and then num_attr_selected > 0 then
         Raven.Event.emit_error ("--raw must be used without selecting other attributes.");
         return False;
      end if;

      --  Read information from files's retrieved manifest.
      declare
         given_path    : constant String := USS (comline.cmd_info.path_archive_file);
         resolved_path : constant String := Archive.Unix.real_path (given_path);
         operation     : Archive.Unpack.Darc;
         metatree      : ThickUCL.UclTree;
      begin
         if IsBlank (resolved_path) then
            Raven.Event.emit_error ("No such package: " & given_path);
            return False;
         end if;

         operation.open_rvn_archive (resolved_path, Archive.normal);
         begin
            ThickUCL.Files.parse_ucl_string (metatree, operation.extract_metadata, "");
         exception
            when ThickUCL.Files.ucl_file_unparseable =>
               Raven.Event.emit_error ("Fatal error: Failed to obtain packages metadata.");
               operation.close_rvn_archive;
               return False;
         end;
         operation.close_rvn_archive;

         if cmd.raw_manifest then
            TIO.Put (ThickUCL.Emitter.emit_ucl (metatree));
            return True;
         end if;

         if cmd.full_information or else num_attr_selected = 0 then
            display_full_information (metatree, resolved_path);
            return True;
         end if;

         --  [Known single package scenario]
         --  if it's a single attribute, just print the value
         --  If it's two or more attributes, use format "<attr>      : <value>"

         display_individual_attributes (metatree  => metatree,
                                        comline   => comline,
                                        num_attr  => num_attr_selected,
                                        rvn_path  => resolved_path);
         return True;
      end;
   end execute_info_command;


   ----------------------------------------
   --  display_individual_attributes #1  --
   ----------------------------------------
   procedure display_individual_attributes
     (metatree  : ThickUCL.UclTree;
      comline   : Cldata;
      num_attr  : Natural;
      rvn_path  : String)
   is
      single : Boolean := num_attr = 1;
      Q      : Boolean := comline.common_options.quiet;
      info   : Raven.Cmd.switches_info_cmd renames comline.cmd_info;
   begin
      display_string (metatree, info.namebase, single, Q, MET.namebase);
      display_string (metatree, info.subpackage, single, Q, MET.subpackage);
      display_string (metatree, info.variant, single, Q, MET.variant);
      display_string (metatree, info.abi, single, Q, MET.abi);
      display_string (metatree, info.comment, single, Q, MET.comment);
      display_string (metatree, info.install_prefix, single, Q, MET.prefix);
      display_string (metatree, info.description, single, Q, MET.description);
      display_size   (metatree, info.total_size, single, Q);
      display_array  (metatree, info.shlibs_used, single, Q, MET.shlibs_required);
      display_array  (metatree, info.shlibs_provided, single, Q, MET.shlibs_provided);
      display_array  (metatree, info.shlibs_adjacent, single, Q, MET.shlibs_adjacent);
      display_dependencies (metatree, info.dependencies, single, Q);
      display_annotations  (metatree, info.annotations, single, Q);
      display_options  (metatree, info.options, single, Q);
      display_message  (metatree, info.install_message, single, Q, Pkgtypes.install);
      display_message  (metatree, info.remove_message, single, Q, Pkgtypes.deinstall);
      list_files       (rvn_path, info.list_files, single, Q, False, False);
      list_files       (rvn_path, info.list_digests, single, Q, True, False);
      list_files       (rvn_path, info.list_attributes, single, Q, False, True);
   end display_individual_attributes;


   ----------------------------------------
   --  display_individual_attributes #2  --
   ----------------------------------------
   procedure display_individual_attributes
     (mpkg      : Pkgtypes.A_Package;
      comline   : Cldata;
      num_attr  : Natural)
   is
      single : Boolean := num_attr = 1;
      Q      : Boolean := comline.common_options.quiet;
      info   : Raven.Cmd.switches_info_cmd renames comline.cmd_info;
   begin
      display_string (USS (mpkg.namebase), info.namebase, single, Q, MET.namebase);
      display_string (USS (mpkg.subpackage), info.subpackage, single, Q, MET.subpackage);
      display_string (USS (mpkg.variant), info.variant, single, Q, MET.variant);
      display_string (USS (mpkg.abi), info.abi, single, Q, MET.abi);
      display_string (USS (mpkg.comment), info.comment, single, Q, MET.comment);
      display_string (USS (mpkg.prefix), info.install_prefix, single, Q, MET.prefix);
      display_string (USS (mpkg.desc), info.description, single, Q, MET.description);
      display_size   (mpkg.flatsize, info.total_size, single, Q);
      display_array  (mpkg.libs_required, info.shlibs_used, single, Q, MET.shlibs_required);
      display_array  (mpkg.libs_provided, info.shlibs_provided, single, Q, MET.shlibs_provided);
      display_array  (mpkg.libs_adjacent, info.shlibs_adjacent, single, Q, MET.shlibs_adjacent);
      display_dependencies    (mpkg.dependencies, info.dependencies, single, Q);
      display_annotations     (mpkg.annotations, info.annotations, single, Q);
      display_options  (mpkg.options, info.options, single, Q);
      display_message  (mpkg, info.install_message, single, Q, Pkgtypes.install);
      display_message  (mpkg, info.remove_message, single, Q, Pkgtypes.deinstall);
      list_files       (mpkg, info.list_files, single, Q, False);
      list_files       (mpkg, info.list_digests, single, Q, True);
   end display_individual_attributes;


   --------------------
   --  format_label  --
   --------------------
   function format_label (label_text : String) return attr_label
   is
      result : attr_label := (others => ' ');
   begin
      if label_text'Length > attr_label'Length then
         result (result'Range) :=
           label_text (label_text'First .. label_text'First + result'Length - 1);
      else
         result (result'First .. result'First + label_text'Length - 1) := label_text;
      end if;
      return result;
   end format_label;


   -------------------------
   --  display_string #1  --
   -------------------------
   procedure display_string
     (metatree : ThickUCL.UclTree;
      active   : Boolean;
      single   : Boolean;
      quiet    : Boolean;
      mfield   : MET.metadata_field)
   is
      key        : constant String := MET.metadata_field_label (mfield);
      data_label : constant String := MET.metadata_field_formal_label (mfield);
      this_label : constant attr_label := format_label (data_label);

      procedure print_string
      is
         dtype : ThickUCL.Leaf_type;
      begin
         dtype := ThickUCL.get_data_type (metatree, key);
         case dtype is
            when ThickUCL.data_string      => TIO.Put_Line (metatree.get_base_value (key));
            when ThickUCL.data_not_present => TIO.Put_Line (not_present_in_metadata);
            when others                    => TIO.Put_Line (wrong_type);
         end case;
      end print_string;
   begin
      if not active then
         return;
      end if;
      if single or else quiet then
         print_string;
      else
         case mfield is
            when MET.description =>
               TIO.Put_Line (this_label & ":");
               print_string;
               TIO.Put_Line ("");  --  intentionally insert linefeed if not single
            when others =>
               TIO.Put (this_label & ": ");
               print_string;
         end case;
      end if;
   end display_string;


   -------------------------
   --  display_string #2  --
   -------------------------
   procedure display_string
     (line     : String;
      active   : Boolean;
      single   : Boolean;
      quiet    : Boolean;
      mfield   : MET.metadata_field)
   is
      data_label : constant String := MET.metadata_field_formal_label (mfield);
      this_label : constant attr_label := format_label (data_label);
   begin
      if not active then
         return;
      end if;
      if single or else quiet then
         TIO.Put_Line (line);
      else
         case mfield is
            when MET.description =>
               TIO.Put_Line (this_label & ":");
               TIO.Put_Line (line & LAT.LF);  --  intentionally insert line feed if not single
            when others =>
               TIO.Put_Line (this_label & ": " & line);
         end case;
      end if;
   end display_string;


   ------------------------
   --  display_array #1  --
   ------------------------
   procedure display_array
     (metatree : ThickUCL.UclTree;
      active   : Boolean;
      single   : Boolean;
      quiet    : Boolean;
      mfield   : MET.metadata_field)
   is
      dtype      : ThickUCL.Leaf_type;
      key        : constant String := MET.metadata_field_label (mfield);
      data_label : constant String := MET.metadata_field_formal_label (mfield);
      this_label : constant attr_label := format_label (data_label);
      nelements  : Natural;
      vndx       : ThickUCL.array_index;
   begin
      if not active then
         return;
      end if;
      dtype := ThickUCL.get_data_type (metatree, key);
      case dtype is
         when ThickUCL.data_array =>
            vndx := metatree.get_index_of_base_array (key);
            nelements := metatree.get_number_of_array_elements (vndx);
            for index in 0 .. nelements - 1 loop
               case metatree.get_array_element_type (vndx, index) is
                  when ThickUCL.data_string =>
                     declare
                        val : constant String := metatree.get_array_element_value (vndx, index);
                     begin
                        if single or else quiet
                        then
                           TIO.Put_Line (val);
                        else
                           if index = 0 then
                              TIO.Put_Line (this_label & ": " & val);
                           else
                              TIO.Put_Line (no_label & ": " & val);
                           end if;
                        end if;
                     end;
                  when others => null;
               end case;
            end loop;
         when ThickUCL.data_not_present => TIO.Put_Line (not_present_in_metadata);
         when others                    => TIO.Put_Line (wrong_type);
      end case;
   end display_array;


   ------------------------
   --  display_array #2  --
   ------------------------
   procedure display_array
     (tarray   : Pkgtypes.Text_List.Vector;
      active   : Boolean;
      single   : Boolean;
      quiet    : Boolean;
      mfield   : MET.metadata_field)
   is
      data_label : constant String := MET.metadata_field_formal_label (mfield);
      this_label : constant attr_label := format_label (data_label);
      counter    : Natural := 0;

      procedure print (Position : Pkgtypes.Text_List.Cursor)
      is
         value : constant String := USS (Pkgtypes.Text_List.Element (Position));
      begin
         if single or else quiet
         then
            TIO.Put_Line (value);
         else
            if counter = 0 then
               TIO.Put_Line (this_label & ": " & value);
            else
               TIO.Put_Line (no_label & ": " & value);
            end if;
         end if;
         counter := counter + 1;
      end print;
   begin
      if not active then
         return;
      end if;
      tarray.Iterate (Print'Access);
   end display_array;


   -----------------------------
   --  display_array_oneline  --
   -----------------------------
   procedure display_array_oneline
     (metatree : ThickUCL.UclTree;
      mfield   : MET.metadata_field;
      active   : Boolean;
      single   : Boolean;
      quiet    : Boolean)
   is
      dtype      : ThickUCL.Leaf_type;
      key        : constant String := MET.metadata_field_label (mfield);
      data_label : constant String := MET.metadata_field_formal_label (mfield);
      this_label : constant attr_label := format_label (data_label);
      nelements  : Natural;
      vndx       : ThickUCL.array_index;
      values     : Text;
   begin
      if not active then
         return;
      end if;
      if not (single and then quiet) then
         TIO.Put (this_label & ": ");
      end if;
      dtype := ThickUCL.get_data_type (metatree, key);
      case dtype is
         when ThickUCL.data_array =>
            vndx := metatree.get_index_of_base_array (key);
            nelements := metatree.get_number_of_array_elements (vndx);
            for index in 0 .. nelements - 1 loop
               case metatree.get_array_element_type (vndx, index) is
                  when ThickUCL.data_string =>
                     declare
                        val : constant String := metatree.get_array_element_value (vndx, index);
                     begin
                        if IsBlank (values) then
                           values := SUS (val);
                        else
                           SU.Append (values, ", " & val);
                        end if;
                     end;
                  when others => null;
               end case;
            end loop;
         when others => null;
      end case;
      TIO.Put_Line (USS (values));
   end display_array_oneline;


   -----------------------------
   --  display_array_oneline  --
   -----------------------------
   procedure display_array_oneline
     (tarray   : Pkgtypes.Text_List.Vector;
      mfield   : MET.metadata_field;
      active   : Boolean;
      single   : Boolean;
      quiet    : Boolean)
   is
      data_label : constant String := MET.metadata_field_formal_label (mfield);
      this_label : constant attr_label := format_label (data_label);
      line       : Text := SU.Null_Unbounded_String;

      procedure print (Position : Pkgtypes.Text_List.Cursor)
      is
         value : constant String := USS (Pkgtypes.Text_List.Element (Position));
      begin
         SU.Append (line, value & " ");
      end print;
   begin
      if not active then
         return;
      end if;
      tarray.Iterate (print'Access);
      if single or else quiet then
         TIO.Put_Line (USS (line));
      else
         TIO.Put_Line (this_label & ": " & USS (line));
      end if;
   end display_array_oneline;


   -----------------------
   --  display_size #1  --
   -----------------------
   procedure display_size
     (metatree : ThickUCL.UclTree;
      active   : Boolean;
      single   : Boolean;
      quiet    : Boolean)
   is
      data_label : constant String := MET.metadata_field_formal_label (MET.flatsize);
      this_label : constant attr_label := format_label (data_label);
   begin
      if not active then
         return;
      end if;
      declare
         size_value : constant String :=
           MET.human_readable_size (MET.get_size (metatree, MET.flatsize));
      begin
         if single or else quiet then
            TIO.Put_Line (size_value);
         else
            TIO.Put_Line (this_label & ": " & size_value);
         end if;
      end;
   end display_size;


   ----------------------
   --  display_size #2 --
   ----------------------
   procedure display_size
     (value    : Pkgtypes.Package_Size;
      active   : Boolean;
      single   : Boolean;
      quiet    : Boolean)
   is
      data_label : constant String := MET.metadata_field_formal_label (MET.flatsize);
      this_label : constant attr_label := format_label (data_label);
      size_value : constant String := MET.human_readable_size (int64 (value));
   begin
      if not active then
         return;
      end if;
      if single or else quiet then
         TIO.Put_Line (size_value);
      else
         TIO.Put_Line (this_label & ": " & size_value);
      end if;
   end display_size;


   --------------------------
   --  display_message #1  --
   --------------------------
   procedure display_message
     (metatree : ThickUCL.UclTree;
      active   : Boolean;
      single   : Boolean;
      quiet    : Boolean;
      mtype    : Pkgtypes.Message_Type)
   is
      key : constant String := MET.metadata_field_label (MET.messages);

      function make_label return attr_label is
      begin
         case mtype is
            when Pkgtypes.install   => return format_label ("install msgs");
            when Pkgtypes.deinstall => return format_label ("deinstall msgs");
            when Pkgtypes.upgrade   => return format_label ("upgrade msgs");
         end case;
      end make_label;
   begin
      if not active then
         return;
      end if;
      declare
         this_label : constant attr_label := make_label;
         msg : constant String := MET.get_message (metatree, mtype);
      begin
         if not IsBlank (msg) then
            TIO.Put_Line (this_label & ":");
            TIO.Put (msg);
         end if;
      end;
   end display_message;


   --------------------------
   --  display_message #2  --
   --------------------------
   procedure display_message
     (mpkg     : Pkgtypes.A_Package;
      active   : Boolean;
      single   : Boolean;
      quiet    : Boolean;
      mtype    : Pkgtypes.Message_Type)
   is
      function make_label return attr_label is
      begin
         case mtype is
            when Pkgtypes.install   => return format_label ("install msgs");
            when Pkgtypes.deinstall => return format_label ("deinstall msgs");
            when Pkgtypes.upgrade   => return format_label ("upgrade msgs");
         end case;
      end make_label;

      this_label : constant attr_label := make_label;
      msg : Text;

      procedure print (Position : Pkgtypes.Message_List.Cursor)
      is
         payload : constant String := USS (Pkgtypes.Message_List.Element (Position).message);
      begin
         if payload (payload'Last) = LAT.LF then
            SU.Append (msg, payload);
         else
            SU.Append (msg, payload & LAT.LF);
         end if;
      end print;
   begin
      if not active then
         return;
      end if;

      mpkg.messages (mtype).Iterate (print'Access);
      if not IsBlank (msg) then
            TIO.Put_Line (this_label & ":");
            TIO.Put (USS (msg));
         end if;
   end display_message;



   -------------------------------
   --  display_dependencies #1  --
   -------------------------------
   procedure display_dependencies
     (metatree : ThickUCL.UclTree;
      active   : Boolean;
      single   : Boolean;
      quiet    : Boolean)
   is
      procedure print (Position : ThickUCL.jar_string.Cursor);
      tree_key   : constant String := MET.metadata_field_label (MET.dependencies);
      data_label : constant String := MET.metadata_field_formal_label (MET.dependencies);
      this_label : constant attr_label := format_label (data_label);
      dep_keys   : ThickUCL.jar_string.Vector;
      ondx       : ThickUCL.object_index;
      counter    : Natural := 0;

      procedure print (Position : ThickUCL.jar_string.Cursor)
      is
         dependency : constant String := USS (ThickUCL.jar_string.Element (Position).payload);
         version    : constant String := metatree.get_object_value (ondx, dependency);
         line       : constant String := dependency & "-" & version;
      begin
         if single or else quiet then
            TIO.Put_Line (line);
         else
            if counter = 0 then
               TIO.Put_Line (this_label & ": " & line);
            else
               TIO.Put_Line (no_label & ": " & line);
            end if;
         end if;
         counter := counter + 1;
      end print;
   begin
      if not active then
         return;
      end if;
      MET.obtain_dependencies_keys (metatree, dep_keys);
      ondx := metatree.get_index_of_base_ucl_object (tree_key);
      dep_keys.Iterate (print'Access);
   end display_dependencies;


   -------------------------------
   --  display_dependencies #2  --
   -------------------------------
   procedure display_dependencies
     (dmap     : Pkgtypes.NV_Pairs.Map;
      active   : Boolean;
      single   : Boolean;
      quiet    : Boolean)
   is
      data_label : constant String := MET.metadata_field_formal_label (MET.dependencies);
      this_label : constant attr_label := format_label (data_label);
      counter    : Natural := 0;

      procedure print (Position : Pkgtypes.NV_Pairs.Cursor)
      is
         line : constant String := USS (Pkgtypes.NV_Pairs.Key (Position));
      begin
         if single or else quiet
         then
            TIO.Put_Line (line);
         else
            if counter = 0 then
               TIO.Put_Line (this_label & ": " & line);
            else
               TIO.Put_Line (no_label & ": " & line);
            end if;
         end if;
         counter := counter + 1;
      end print;
   begin
      if not active then
         return;
      end if;
      dmap.Iterate (Print'Access);
   end display_dependencies;


   --------------------------
   --  display_options #1  --
   --------------------------
   procedure display_options
     (metatree : ThickUCL.UclTree;
      active   : Boolean;
      single   : Boolean;
      quiet    : Boolean)
   is
      procedure print (Position : ThickUCL.jar_string.Cursor);

      key        : constant String := MET.metadata_field_label (MET.options);
      data_label : constant String := MET.metadata_field_formal_label (MET.options);
      this_label : constant attr_label := format_label (data_label);
      opt_keys   : ThickUCL.jar_string.Vector;
      dtype      : ThickUCL.Leaf_type;
      vndx       : ThickUCL.object_index;
      counter    : Natural := 0;

      procedure print (Position : ThickUCL.jar_string.Cursor)
      is
         optname : constant String := USS (ThickUCL.jar_string.Element (Position).payload);
         ntype   : ThickUCL.Leaf_type;

         function interpret_optvalue return String
         is
            answer : Boolean;
            option_is_set : constant String := "true";
         begin
            case metatree.get_object_data_type (vndx, optname) is
               when ThickUCL.data_boolean =>
                  answer := metatree.get_object_value (vndx, optname);
                  if answer then
                     return option_is_set;
                  end if;
               when ThickUCL.data_string =>
                  declare
                     answerstr : constant String :=
                       lowercase (metatree.get_object_value (vndx, optname));
                  begin
                     if answerstr = "on" or else answerstr = "true" then
                        return option_is_set;
                     end if;
                  end;
               when others => null;
            end case;
            return "false";
         end interpret_optvalue;
      begin
         ntype := metatree.get_object_data_type (vndx, optname);
         case ntype is
            when ThickUCL.data_string |
                 ThickUCL.data_boolean =>
               declare
                  optvalue : constant String := interpret_optvalue;
                  line     : constant String := optname & " => " & optvalue;
               begin
                  if single or else quiet then
                     TIO.Put_Line (line);
                  else
                     if counter = 0 then
                        TIO.Put_Line (this_label & ": " & line);
                     else
                        TIO.Put_Line (no_label & ": " & line);
                     end if;
                  end if;
               end;
               counter := counter + 1;
            when others =>
               null;
         end case;
      end print;
   begin
      if not active then
         return;
      end if;
      dtype := metatree.get_data_type (key);
      case dtype is
         when ThickUCL.data_object =>
            vndx := metatree.get_index_of_base_ucl_object (key);
            MET.obtain_options_keys (metatree, opt_keys);
            opt_keys.Iterate (print'Access);
         when others =>
            null;
      end case;
   end display_options;


   --------------------------
   --  display_options #2  --
   --------------------------
   procedure display_options
     (dmap     : Pkgtypes.NV_Pairs.Map;
      active   : Boolean;
      single   : Boolean;
      quiet    : Boolean)
   is
      data_label : constant String := MET.metadata_field_formal_label (MET.options);
      this_label : constant attr_label := format_label (data_label);
      counter    : Natural := 0;

      procedure print (Position : Pkgtypes.NV_Pairs.Cursor)
      is
         key   : Text renames Pkgtypes.NV_Pairs.Key (Position);
         value : Text renames Pkgtypes.NV_Pairs.Element (Position);
         line  : constant String := USS (key) & " => " & USS (value);
      begin
         if single or else quiet
         then
            TIO.Put_Line (line);
         else
            if counter = 0 then
               TIO.Put_Line (this_label & ": " & line);
            else
               TIO.Put_Line (no_label & ": " & line);
            end if;
         end if;
         counter := counter + 1;
      end print;
   begin
      if not active then
         return;
      end if;
      dmap.Iterate (Print'Access);
   end display_options;


   ------------------------------
   --  display_annotations #1  --
   ------------------------------
   procedure display_annotations
     (metatree : ThickUCL.UclTree;
      active   : Boolean;
      single   : Boolean;
      quiet    : Boolean)
   is
      procedure print (Position : ThickUCL.jar_string.Cursor);

      key        : constant String := MET.metadata_field_label (MET.annotations);
      data_label : constant String := MET.metadata_field_formal_label (MET.annotations);
      this_label : constant attr_label := format_label (data_label);
      note_keys  : ThickUCL.jar_string.Vector;
      dtype      : ThickUCL.Leaf_type;
      vndx       : ThickUCL.object_index;
      counter    : Natural := 0;

      procedure print (Position : ThickUCL.jar_string.Cursor)
      is
         note_id : constant String := USS (ThickUCL.jar_string.Element (Position).payload);
         ntype   : ThickUCL.Leaf_type;
      begin
         ntype := metatree.get_object_data_type (vndx, note_id);
         case ntype is
            when ThickUCL.data_string =>
               declare
                  note : constant String := metatree.get_object_value (vndx, note_id);
               begin
                  if single or else quiet then
                     TIO.Put_Line (note_id & " = " & note);
                  else
                     if counter = 0 then
                        TIO.Put_Line (this_label & ": " & note_id & " => " & note);
                     else
                        TIO.Put_Line (no_label & ": " & note_id & " => " & note);
                     end if;
                  end if;
               end;
               counter := counter + 1;
            when others =>
               null;
         end case;
      end print;
   begin
      if not active then
         return;
      end if;
      dtype := metatree.get_data_type (key);
      case dtype is
         when ThickUCL.data_object =>
            vndx := metatree.get_index_of_base_ucl_object (key);
            MET.obtain_annotations_keys (metatree, note_keys);
            note_keys.Iterate (print'Access);
         when others =>
            null;
      end case;
   end display_annotations;


   ------------------------------
   --  display_annotations #2  --
   ------------------------------
   procedure display_annotations
     (dmap     : Pkgtypes.NoteSet.Map;
      active   : Boolean;
      single   : Boolean;
      quiet    : Boolean)
   is
      data_label : constant String := MET.metadata_field_formal_label (MET.annotations);
      this_label : constant attr_label := format_label (data_label);
      counter    : Natural := 0;

      procedure print (Position : Pkgtypes.NoteSet.Cursor)
      is
         mynote : Pkgtypes.Note_Item renames Pkgtypes.NoteSet.Element (Position);
         key   : constant String := USS (mynote.tag);
         value : constant String := USS (mynote.note);
         line  : constant String := key & " => " & value;
      begin
         if single or else quiet
         then
            TIO.Put_Line (line);
         else
            if counter = 0 then
               TIO.Put_Line (this_label & ": " & line);
            else
               TIO.Put_Line (no_label & ": " & line);
            end if;
         end if;
         counter := counter + 1;
      end print;
   begin
      if not active then
         return;
      end if;
      dmap.Iterate (Print'Access);
   end display_annotations;


   ---------------------
   --  list_files #1  --
   ---------------------
   procedure list_files
     (rvn_path  : String;
      active    : Boolean;
      single    : Boolean;
      quiet     : Boolean;
      digest    : Boolean;
      extended  : Boolean)
   is
      this_label : constant attr_label := format_label ("files");
      operation  : Archive.Unpack.Darc;
      indent     : Natural := attr_label'Length + 2;
   begin
      if not active then
         return;
      end if;
      if single or else quiet then
         indent := 0;
      end if;
      if not single then
         TIO.Put_Line (this_label & ":");
      end if;
      --  Perhaps we will want to update the Archive.Unpack class to configure a 4-character
      --  long indent for the files list.  Right now they aren't indented.
      operation.open_rvn_archive (rvn_path, Archive.normal);
      operation.print_manifest (digest, extended, indent);
      operation.close_rvn_archive;
   end list_files;


   ---------------------
   --  list_files #2  --
   ---------------------
   procedure list_files
     (mpkg     : Pkgtypes.A_Package;
      active   : Boolean;
      single   : Boolean;
      quiet    : Boolean;
      digests  : Boolean)
   is
      counter    : Natural := 0;
      this_label : constant attr_label := format_label ("files");

      procedure print (Position : Pkgtypes.File_List.Cursor)
      is
         myrec : Pkgtypes.File_Item renames Pkgtypes.File_List.Element (Position);

         function line return String is
         begin
            if digests then
               return myrec.digest & " " & USS (myrec.path);
            end if;
            return USS (myrec.path);
         end line;
      begin
         if single or else quiet
         then
            TIO.Put_Line (line);
         else
            if counter = 0 then
               TIO.Put_Line (this_label & ": " & line);
            else
               TIO.Put_Line (no_label & ": " & line);
            end if;
         end if;
         counter := counter + 1;
      end print;
   begin
      if not active then
         return;
      end if;
      mpkg.files.Iterate (Print'Access);
   end list_files;


   -----------------------------------
   --  display_full_information #1  --
   -----------------------------------
   procedure display_full_information
     (metatree : ThickUCL.UclTree;
      pkg_path : String)
   is
      quiet    : constant Boolean := False;
      basename : constant String := head (tail (pkg_path, "/"), ".");
   begin
      TIO.Put_Line (basename);
      display_string (metatree, True, False, quiet, MET.namebase);
      display_string (metatree, True, False, quiet, MET.subpackage);
      display_string (metatree, True, False, quiet, MET.variant);
      display_string (metatree, True, False, quiet, MET.version);
      display_string (metatree, True, False, quiet, MET.abi);
      display_string (metatree, True, False, quiet, MET.comment);
      display_string (metatree, True, False, quiet, MET.website);

      display_array_oneline (metatree, MET.categories, True, False, quiet);
      display_array_oneline (metatree, MET.licenses, True, False, quiet);

      display_string (metatree, True, False, quiet, MET.license_logic);
      display_string (metatree, True, False, quiet, MET.maintainer);
      display_string (metatree, True, False, quiet, MET.prefix);

      display_dependencies (metatree, True, False, quiet);
      display_options      (metatree, True, False, quiet);

      display_array (metatree, True, False, quiet, MET.shlibs_required);
      display_array (metatree, True, False, quiet, MET.shlibs_provided);
      display_array (metatree, True, False, quiet, MET.shlibs_adjacent);

      display_annotations (metatree, True, False, quiet);
      display_size        (metatree, True, False, quiet);
      display_string      (metatree, True, False, quiet, MET.description);
   end display_full_information;


   -----------------------------------
   --  display_full_information #2  --
   -----------------------------------
   procedure display_full_information (mpkg : Pkgtypes.A_Package)
   is
      quiet : constant Boolean := False;
      logic : constant String := MET.get_license_scheme (mpkg.licenselogic);
   begin
      TIO.Put_Line (Pkgtypes.nsvv_identifier (mpkg));
      display_string (USS (mpkg.namebase), True, False, quiet, MET.namebase);
      display_string (USS (mpkg.subpackage), True, False, quiet, MET.subpackage);
      display_string (USS (mpkg.variant), True, False, quiet, MET.variant);
      display_string (USS (mpkg.version), True, False, quiet, MET.version);
      display_string (USS (mpkg.abi), True, False, quiet, MET.abi);
      display_string (USS (mpkg.comment), True, False, quiet, MET.comment);
      display_string (USS (mpkg.www), True, False, quiet, MET.website);

      display_array_oneline (mpkg.categories, MET.categories, True, False, quiet);
      display_array_oneline (mpkg.licenses, MET.licenses, True, False, quiet);

      display_string (logic, True, False, quiet, MET.license_logic);
      display_string (USS (mpkg.maintainer), True, False, quiet, MET.maintainer);
      display_string (USS (mpkg.prefix), True, False, quiet, MET.prefix);

      display_dependencies (mpkg.dependencies, True, False, quiet);
      display_options      (mpkg.options, True, False, quiet);

      display_array (mpkg.libs_required, True, False, quiet, MET.shlibs_required);
      display_array (mpkg.libs_provided, True, False, quiet, MET.shlibs_provided);
      display_array (mpkg.libs_adjacent, True, False, quiet, MET.shlibs_adjacent);

      display_annotations (mpkg.annotations, True, False, quiet);
      display_size        (mpkg.flatsize, True, False, quiet);
      display_string      (USS (mpkg.desc), True, False, quiet, MET.description);
   end display_full_information;


   --------------------------------------
   --  execute_installed_info_command  --
   --------------------------------------
   function execute_installed_info_command (comline : Cldata;
                                            num_attr_selected : Natural) return Boolean
   is
      rdb : Database.RDB_Connection;
      unfinished_packages : Pkgtypes.Package_Set.Vector;
      finished_packages : Pkgtypes.Package_Set.Vector;
      behave_cs : Boolean := comline.common_options.case_sensitive;

      procedure finish (Position : Pkgtypes.Package_Set.Cursor)
      is
         mypkg : Pkgtypes.A_Package := Pkgtypes.Package_Set.Element (Position);
      begin
         Database.Query.finish_package (rdb, mypkg);
         finished_packages.Append (mypkg);
      end finish;

      procedure show_package_info (Position : Pkgtypes.Package_Set.Cursor)
      is
         mypkg : Pkgtypes.A_Package renames Pkgtypes.Package_Set.Element (Position);
      begin
         if comline.cmd_info.full_information or else num_attr_selected = 0 then
            display_full_information (mypkg);
            return;
         end if;

         display_individual_attributes (mypkg, comline, num_attr_selected);
      end show_package_info;

   begin
      if Context.reveal_case_sensitive then
         behave_cs := True;
      end if;

      case OPS.rdb_open_localdb (rdb, Database.installed_packages) is
         when RESULT_OK => null;
         when others => return False;
      end case;

      if not LOK.obtain_lock (rdb, LOK.lock_readonly) then
         Event.emit_error (LOK.no_read_lock);
         OPS.rdb_close (rdb);
         return False;
      end if;

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

      unfinished_packages.Iterate (finish'Access);

      if not LOK.release_lock (rdb, LOK.lock_readonly) then
         Event.emit_error (LOK.no_read_unlock);
         OPS.rdb_close (rdb);
         return False;
      end if;

      OPS.rdb_close (rdb);

      if comline.cmd_info.installed then
         RCU.override_exit_status;
         if finished_packages.Is_Empty then
            Ada.Command_Line.Set_Exit_Status (1);
         else
            Ada.Command_Line.Set_Exit_Status (0);
         end if;
      end if;

      finished_packages.Iterate (show_package_info'Access);
      return True;

   end execute_installed_info_command;


end Raven.Cmd.Info;
