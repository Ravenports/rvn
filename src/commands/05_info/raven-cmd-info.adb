--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../../License.txt


with Ada.Command_Line;
with Archive.Unix;
with Archive.Unpack;
with ThickUCL.Files;
with ThickUCL.Emitter;
with Raven.Cmd.Unset;
with Raven.Event;
with Raven.Strings; Use Raven.Strings;

package body Raven.Cmd.Info is

   package RCU renames Raven.Cmd.Unset;

   -----------------------------
   --  Executes info command  --
   -----------------------------
   function execute_info_command (comline : Cldata) return Boolean
   is
      procedure increment (attribute : Boolean);
      procedure check_installed;

      num_attr_selected : Natural := 0;
      single_pkg_installed : Boolean := False;
      multiple_packages_found : Boolean := False;
      cmd : switches_info_cmd renames comline.cmd_info;

      procedure increment (attribute : Boolean) is
      begin
         if attribute then
            num_attr_selected := num_attr_selected + 1;
         end if;
      end increment;

      procedure check_installed
      is
         --  Pre-requisites
         --  1) not used with -F pkg-file option (makes no sense)
         --  2) Not used if multiple packages are returned in the query
      begin
         if cmd.installed then
            RCU.override_exit_status;
            if multiple_packages_found then
               Ada.Command_Line.Set_Exit_Status (2);
               Raven.Event.emit_error ("Multiple packages found; --exists can't be determined.");
            else
               if single_pkg_installed then
                  Ada.Command_Line.Set_Exit_Status (1);
               else
                  Ada.Command_Line.Set_Exit_Status (0);
               end if;
            end if;
         end if;
      end check_installed;
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
      increment (cmd.namebase);
      increment (cmd.rev_deps);
      increment (cmd.shlibs_adjacent);
      increment (cmd.shlibs_provided);
      increment (cmd.shlibs_used);
      increment (cmd.subpackage);
      increment (cmd.total_size);
      increment (cmd.variant);

      if IsBlank (comline.cmd_info.path_archive_file) then
         Raven.Event.emit_error
           ("The info command for installed packages has not been implemented yet");
         return False;
      end if;

      if cmd.installed and then num_attr_selected > 0 then
         Raven.Event.emit_error ("--raw must be used without any other selected attributes.");
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
            display_full_information (metatree, resolved_path, comline.common_options.quiet);
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


   -------------------------------------
   --  display_individual_attributes  --
   -------------------------------------
   procedure display_individual_attributes
     (metatree  : ThickUCL.UclTree;
      comline   : Cldata;
      num_attr  : Natural;
      rvn_path  : String)
   is
      single : Boolean := num_attr = 1;
      Q      : Boolean := comline.common_options.quiet;
   begin
      display_string (metatree, comline.cmd_info.namebase, single, MET.namebase);
      display_string (metatree, comline.cmd_info.subpackage, single, MET.subpackage);
      display_string (metatree, comline.cmd_info.variant, single, MET.variant);
      display_string (metatree, comline.cmd_info.comment, single, MET.comment);
      display_string (metatree, comline.cmd_info.install_prefix, single, MET.prefix);
      display_string (metatree, comline.cmd_info.description, single, MET.description);
      display_size   (metatree, comline.cmd_info.total_size, single);
      display_array  (metatree, comline.cmd_info.shlibs_used, single, Q, MET.shlibs_required);
      display_array  (metatree, comline.cmd_info.shlibs_provided, single, Q, MET.shlibs_provided);
      display_array  (metatree, comline.cmd_info.shlibs_adjacent, single, Q, MET.shlibs_adjacent);
      display_dependencies    (metatree, comline.cmd_info.dependencies, single, Q);
      display_annotations     (metatree, comline.cmd_info.annotations, single, Q);
      display_install_message (metatree, comline.cmd_info.install_message, single);
      list_files              (rvn_path, comline.cmd_info.list_files, single, Q, False, False);
      list_files              (rvn_path, comline.cmd_info.list_digests, single, Q, True, False);
      list_files              (rvn_path, comline.cmd_info.list_attributes, single, Q, False, True);
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


   ----------------------
   --  display_string  --
   ----------------------
   procedure display_string
     (metatree : ThickUCL.UclTree;
      active   : Boolean;
      single   : Boolean;
      mfield   : MET.metadata_field)
   is
      dtype      : ThickUCL.Leaf_type;
      key        : constant String := MET.metadata_field_label (mfield);
      this_label : constant attr_label := format_label (key);
   begin
      if not active then
         return;
      end if;
      if not single then
         TIO.Put (this_label & ": ");
         case mfield is
            when MET.description => TIO.Put_Line ("");
            when others => null;
         end case;
      end if;
      dtype := ThickUCL.get_data_type (metatree, key);
      case dtype is
         when ThickUCL.data_string      => TIO.Put_Line (metatree.get_base_value (key));
         when ThickUCL.data_not_present => TIO.Put_Line (not_present_in_metadata);
         when others                    => TIO.Put_Line (wrong_type);
      end case;
   end display_string;


   ---------------------
   --  display_array  --
   ---------------------
   procedure display_array
     (metatree : ThickUCL.UclTree;
      active   : Boolean;
      single   : Boolean;
      quiet    : Boolean;
      mfield   : MET.metadata_field)
   is
      dtype      : ThickUCL.Leaf_type;
      key        : constant String := MET.metadata_field_label (mfield);
      this_label : constant attr_label := format_label (key);
      nelements  : Natural;
      vndx       : ThickUCL.array_index;
   begin
      if not active then
         return;
      end if;
      if not single then
         TIO.Put_line (this_label & ":");  --  All arrays start on a fresh line
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
                        if single or else quiet then
                           TIO.Put_Line (val);
                        else
                           TIO.Put_Line ("    " & val);
                        end if;
                     end;
                  when others => null;
               end case;
            end loop;
         when ThickUCL.data_not_present => TIO.Put_Line (not_present_in_metadata);
         when others                    => TIO.Put_Line (wrong_type);
      end case;
   end display_array;


   -----------------------------
   --  display_array_oneline  --
   -----------------------------
   procedure display_array_oneline
     (metatree : ThickUCL.UclTree;
      mfield   : MET.metadata_field)
   is
      dtype      : ThickUCL.Leaf_type;
      key        : constant String := MET.metadata_field_label (mfield);
      this_label : constant attr_label := format_label (key);
      nelements  : Natural;
      vndx       : ThickUCL.array_index;
      values     : Text;
   begin
      TIO.Put (this_label & ": ");
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


   --------------------
   --  display_size  --
   --------------------
   procedure display_size
     (metatree : ThickUCL.UclTree;
      active   : Boolean;
      single   : Boolean)
   is
      key        : constant String := MET.metadata_field_label (MET.flatsize);
      this_label : constant attr_label := format_label (key);
   begin
      if not active then
         return;
      end if;
      if not single then
         TIO.Put (this_label & ": ");
      end if;
      TIO.Put_Line (MET.human_readable_size (MET.get_flatsize (metatree)));
   end display_size;


   -------------------------------
   --  display_install_message  --
   -------------------------------
   procedure display_install_message
     (metatree : ThickUCL.UclTree;
      active   : Boolean;
      single   : Boolean)
   is
      key        : constant String := MET.metadata_field_label (MET.flatsize);
      this_label : constant attr_label := format_label (key);
   begin
      if not active then
         return;
      end if;
      if not single then
         TIO.Put_Line (this_label & ":");
      end if;
      declare
         val : constant String := MET.get_message (metatree, MET.install);
      begin
         if not IsBlank (val) then
            TIO.Put_Line (val);
         end if;
      end;
   end display_install_message;


   ----------------------------
   --  display_dependencies  --
   ----------------------------
   procedure display_dependencies
     (metatree : ThickUCL.UclTree;
      active   : Boolean;
      single   : Boolean;
      quiet    : Boolean)
   is
      procedure print (Position : ThickUCL.jar_string.Cursor);

      key        : constant String := MET.metadata_field_label (MET.dependencies);
      this_label : constant attr_label := format_label (key);
      dep_keys   : ThickUCL.jar_string.Vector;

      procedure print (Position : ThickUCL.jar_string.Cursor)
      is
         dependency : constant String := USS (ThickUCL.jar_string.Element (Position).payload);
      begin
         if single or else quiet then
            TIO.Put_Line (dependency);
         end if;
         TIO.Put_Line ("    " & dependency);
      end print;
   begin
      if not active then
         return;
      end if;
      if not single then
         TIO.Put_Line (this_label & ":");
      end if;
      MET.obtain_dependencies_keys (metatree, dep_keys);
      dep_keys.Iterate (print'Access);
   end display_dependencies;


   ---------------------------
   --  display_annotations  --
   ---------------------------
   procedure display_annotations
     (metatree : ThickUCL.UclTree;
      active   : Boolean;
      single   : Boolean;
      quiet    : Boolean)
   is
      procedure print (Position : ThickUCL.jar_string.Cursor);

      key        : constant String := MET.metadata_field_label (MET.annotations);
      this_label : constant attr_label := format_label (key);
      note_keys  : ThickUCL.jar_string.Vector;
      dtype      : ThickUCL.Leaf_type;
      vndx       : ThickUCL.object_index;

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
                  end if;
                  TIO.Put_Line ("    " & note_id & " = " & note);
               end;
            when others =>
               null;
         end case;
      end print;
   begin
      if not active then
         return;
      end if;
      if not single then
         TIO.Put_Line (this_label & ":");
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


   ------------------
   --  list_files  --
   ------------------
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
      indent     : Natural := 4;
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


   --------------------------------
   --  display_full_information  --
   --------------------------------
   procedure display_full_information
     (metatree : ThickUCL.UclTree;
      pkg_path : String;
      quiet    : Boolean)
   is
      basename : constant String := head (tail (pkg_path, "/"), ".");
   begin
      TIO.Put_Line (basename);
      display_string (metatree, True, False, MET.namebase);
      display_string (metatree, True, False, MET.subpackage);
      display_string (metatree, True, False, MET.variant);
      display_string (metatree, True, False, MET.version);
      display_string (metatree, True, False, MET.abi);
      display_string (metatree, True, False, MET.comment);
      display_string (metatree, True, False, MET.website);

      display_array_oneline (metatree, MET.categories);
      display_array_oneline (metatree, MET.licenses);

      display_string (metatree, True, False, MET.maintainer);
      display_string (metatree, True, False, MET.prefix);

      display_array (metatree, True, False, quiet, MET.shlibs_required);
      display_array (metatree, True, False, quiet, MET.shlibs_provided);
      display_array (metatree, True, False, quiet, MET.shlibs_adjacent);

      display_annotations (metatree, True, False, quiet);
      display_size        (metatree, True, False);
      display_string      (metatree, True, False, MET.description);
   end display_full_information;

end Raven.Cmd.Info;
