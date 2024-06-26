--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Text_IO;
with Ada.Directories;
with Raven.Pkgtypes;
with Raven.Strings;
with Raven.Context;
with Raven.Metadata;
with Archive.Misc;
with Lua;

use Raven.Strings;

package body Raven.Triggers is

   package TIO renames Ada.Text_IO;
   package MSC renames Archive.Misc;


   --------------------
   --  trigger_hash  --
   --------------------
   function trigger_hash (trigger_id : Natural) return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type (trigger_id);
   end trigger_hash;


   -----------------------
   --  package_id_hash  --
   -----------------------
   function package_id_hash (pkg_id : Pkgtypes.Package_ID) return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type (pkg_id);
   end package_id_hash;


   -------------------
   --  set_rootdir  --
   -------------------
   procedure set_rootdir (trigger_set  : in out A_Trigger_Set; rootdir : String) is
   begin
      trigger_set.rootdir_txt := SUS (rootdir);
   end set_rootdir;


   --------------
   --  insert  --
   --------------
   procedure insert
     (trigger_set     : in out A_Trigger_Set;
      trigger_id      : Natural;
      trigger_type    : A_Trigger_Type;
      script_code     : String;
      entity_path     : String;
      origin_namebase : String;
      origin_subpkg   : String;
      origin_variant  : String;
      origin_prefix   : String;
      upgrading       : boolean)
   is
      procedure add_argument (Key : Natural; Element : in out TScript) is
      begin
         Element.arguments.Append (SUS (entity_path));
      end add_argument;
   begin
      if trigger_set.trigger_map.Contains (trigger_id) then
         --  ignore trigger_type and script_code.
         --  If programmed correctly, they'll be identical.

         case trigger_set.trigger_map.Element (trigger_id).trigger_type is
            when cleanup      => null;
            when installation =>
               trigger_set.trigger_map.Update_Element
                 (Position => trigger_set.trigger_map.Find (trigger_id),
                  Process  => add_argument'Access);
         end case;
      else
         declare
            myrec : TScript;
         begin
            myrec.arguments.Clear;
            myrec.trigger_type := trigger_type;
            myrec.script_code := SUS (script_code);
            case myrec.trigger_type is
               when cleanup      => null;
               when installation => myrec.arguments.Append (SUS (entity_path));
            end case;
            myrec.origin_namebase := SUS (origin_namebase);
            myrec.origin_subpkg   := SUS (origin_subpkg);
            myrec.origin_variant  := SUS (origin_variant);
            myrec.origin_prefix   := SUS (origin_prefix);
            myrec.upgrading       := upgrading;
            trigger_set.trigger_map.Insert (trigger_id, myrec);
         end;
      end if;
      case trigger_type is
         when cleanup => trigger_set.hit_cleanup := True;
         when installation => null;
      end case;
   end insert;


   ---------------
   --  execute  --
   ---------------
   procedure execute (trigger_set  : in out A_Trigger_Set)
   is
      msg_outfile  : constant String := Lua.unique_msgfile_path;
      std_outfile  : constant String := MSC.new_filename (msg_outfile, MSC.ft_lua);
      out_handle   : TIO.File_Type;
      dummy_handle : TIO.File_Type;

      procedure run_lua_script (Position : A_Trigger_Map.Cursor)
      is
         myscript : TScript renames A_Trigger_Map.Element (Position);
         script_args : Text := SU.Null_Unbounded_String;
         success : Boolean;

         procedure process_arg (Position : Pkgtypes.Text_List.Cursor)
         is
            this_arg : Text renames Pkgtypes.Text_List.Element (Position);
         begin
            if SU.Length (script_args) > 0 then
               SU.Append (script_args, Character'Val (0));
            end if;
            SU.Append (script_args, this_arg);
         end process_arg;
      begin
         case myscript.trigger_type is
            when cleanup => null;
            when installation => myscript.arguments.Iterate (process_arg'Access);
         end case;

         Lua.run_lua_script
           (namebase    => USS (myscript.origin_namebase),
            subpackage  => USS (myscript.origin_subpkg),
            variant     => USS (myscript.origin_variant),
            prefix      => USS (myscript.origin_prefix),
            root_dir    => USS (trigger_set.rootdir_txt),
            upgrading   => myscript.upgrading,
            script      => USS (myscript.script_code),
            arg_chain   => USS (script_args),
            msg_outfile => msg_outfile,
            out_handle  => out_handle,
            success     => success);

      end run_lua_script;
   begin
      begin
         TIO.Create (File => out_handle, Name => std_outfile);
      exception
         when others =>
            null;
      end;
      trigger_set.trigger_map.Iterate (run_lua_script'Access);
      TIO.Close (out_handle);
      Lua.show_post_run_messages (msg_outfile, "N","S","V", dummy_handle);

   end execute;


   --------------------------
   --  gather_directories  --
   --------------------------
   procedure gather_directories
     (trigger_set   : in out A_Trigger_Set;
      installed_pkg : Pkgtypes.A_Package;
      upgrading     : Boolean)
   is
      payload : Text := SU.Null_Unbounded_String;

      function construct_dir (path : Text) return Text
      is
         result : Text := SU.Null_Unbounded_String;
         path_str : constant String := USS (path);
      begin
         if path_str (path_str'First) /= '/' then
            SU.Append (result, installed_pkg.prefix);
            SU.Append (result, '/');
         end if;
         SU.Append (result, head (path, SUS ("/")));
         return result;
      end construct_dir;

      procedure analyze_filename (Position : Pkgtypes.File_List.Cursor)
      is
         myitem : Pkgtypes.File_Item renames Pkgtypes.File_List.Element (Position);
         mydir  : constant Text := construct_dir (myitem.path);
      begin
         if IsBlank (mydir) then
            return;
         end if;
         if not trigger_set.directory_map.Contains (mydir) then
            trigger_set.directory_map.Insert (mydir, payload);
         end if;
      end analyze_filename;

      procedure analyze_directory (Position : Pkgtypes.Text_List.Cursor)
      is
         raw_dir : constant String := USS (Pkgtypes.Text_List.Element (Position));
         prefixed_path : constant String := Metadata.free_directory_path
           (USS (installed_pkg.prefix), raw_dir);
         full_path : constant Text := SUS (prefixed_path);
      begin
         if not trigger_set.directory_map.Contains (full_path) then
            trigger_set.directory_map.Insert (full_path, payload);
         end if;
      end analyze_directory;
   begin
      if upgrading then
         payload := SUS ("upgrade");
      end if;
      if not trigger_set.relevant_pkgs.Contains (installed_pkg.id) then
         trigger_set.relevant_pkgs.Insert (installed_pkg.id, upgrading);
      end if;
      installed_pkg.directories.Iterate (analyze_directory'Access);
      installed_pkg.files.Iterate (analyze_filename'Access);
   end gather_directories;


   ------------------------
   --  dir_path_matches  --
   ------------------------
   function dir_path_matches
     (trigger_set   : A_Trigger_Set;
      dir_path      : String) return Boolean is
   begin
      return trigger_set.directory_map.Contains (SUS (dir_path));
   end dir_path_matches;


   ------------------------
   --  dir_path_upgrade  --
   ------------------------
   function dir_path_upgrade
     (trigger_set   : A_Trigger_Set;
      dir_path      : String) return Boolean
   is
      key : constant Text := SUS (dir_path);
   begin
      if trigger_set.directory_map.Contains (key) then
         return equivalent (trigger_set.directory_map.Element (key), "upgrade");
      end if;
      return False;
   end dir_path_upgrade;


   -----------------------
   --  package_upgrade  --
   -----------------------
   function package_upgrade
     (trigger_set   : A_Trigger_Set;
      pkg_id        : Pkgtypes.Package_ID) return Boolean is
   begin
      if trigger_set.relevant_pkgs.Contains (pkg_id) then
         return trigger_set.relevant_pkgs.Element (pkg_id);
      end if;
      return False;
   end package_upgrade;


   --------------------`
   --  id_selection  --
   --------------------
   function id_selection (trigger_set : A_Trigger_Set) return String
   is
      myset : Text := SUS ("(");
      virgin : Boolean := True;

      procedure construct (Position : A_Pkg_Upgrade_Set.Cursor)
      is
         pkg_id : constant Pkgtypes.Package_ID := A_Pkg_Upgrade_Set.Key (Position);
      begin
         if virgin then
            SU.Append (myset, pkg_id'Img);
            virgin := False;
         else
            SU.Append (myset, ',' & pkg_id'Img);
         end if;
      end construct;
   begin
      trigger_set.relevant_pkgs.Iterate (construct'Access);
      SU.Append (myset, ")");
      return USS (myset);
   end id_selection;


   --------------------
   --  will_cleanup  --
   --------------------
   function will_cleanup (trigger_set : A_Trigger_Set) return Boolean is
   begin
      return trigger_set.hit_cleanup;
   end will_cleanup;

end Raven.Triggers;
