--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Database.Lock;
with Raven.Database.Query;
with Raven.Database.Operations;
with Raven.Strings;
with Raven.Event;
with Archive.Unix;
with Ada.Environment_Variables;

use Raven.Strings;

package body Raven.Cmd.Which is

   package LOK renames Raven.Database.Lock;
   package QRY renames Raven.Database.Query;
   package OPS renames Raven.Database.Operations;
   package ENV renames Ada.Environment_Variables;

   -----------------------------
   --  execute_which_command  --
   -----------------------------
   function execute_which_command (comline : Cldata) return Boolean
   is
      return_result  : Boolean := False;
      search_patterns : Pkgtypes.Text_List.Vector;

      procedure query (Position : Pkgtypes.Text_List.Cursor)
      is
         query_path : constant String := USS (Pkgtypes.Text_List.Element (Position));
         result_pkgs : Pkgtypes.Package_Set.Vector;

         procedure print (Position : Pkgtypes.Package_Set.Cursor)
         is
            this_pkg : Pkgtypes.A_Package renames Pkgtypes.Package_Set.Element (Position);
            pkgname  : constant String := "package " & Pkgtypes.nsvv_identifier (this_pkg);

            procedure print_file (Position : Pkgtypes.File_List.Cursor)
            is
               fitem : Pkgtypes.File_Item renames Pkgtypes.File_List.Element (Position);
            begin
               Event.emit_message (USS (fitem.path));
            end print_file;
         begin
            if comline.common_options.shell_glob then
               Event.emit_message (query_path & " was glob searched and found in " & pkgname);
            else
               Event.emit_message (query_path & " was installed by " & pkgname);
            end if;
            if comline.cmd_which.show_match then
               this_pkg.files.Iterate (print_file'Access);
            end if;
            return_result := True;
         end print;
      begin
         QRY.rvn_which (rdb, query_path, comline.common_options.shell_glob, result_pkgs);
         result_pkgs.Iterate (print'Access);
      end query;
   begin
      case OPS.rdb_open_localdb (rdb) is
         when RESULT_OK => null;
         when others => return False;
      end case;
      if not LOK.obtain_lock (rdb, LOK.lock_readonly) then
         OPS.rdb_close (rdb);
         Event.emit_error ("Cannot get a read lock on a database, it is locked by another process");
         return False;
      end if;

      search_patterns.Append (comline.common_options.name_pattern);
      if comline.cmd_which.path_search then
         add_path_patterns (USS (comline.common_options.name_pattern), search_patterns);
      end if;

      search_patterns.iterate (query'Access);

      if not LOK.release_lock (rdb, LOK.lock_readonly) then
         null;
      end if;
      OPS.rdb_close (rdb);

      return return_result;
   end execute_which_command;


   -------------------------
   --  add_path_patterns  --
   -------------------------
   procedure add_path_patterns (name_pattern : String; patterns : in out Pkgtypes.Text_List.Vector)
   is
      num_paths : Natural := 0;
   begin
      if not ENV.Exists ("PATH") then
         Event.emit_message ("$PATH is not set, falling back to non-search behaviour");
         return;
      end if;
      declare
         env_path : constant String := ENV.Value ("PATH");
      begin
         if IsBlank (env_path) then
            return;
         end if;
         num_paths := count_char (env_path, ':') + 1;
         for field_number in 1 .. num_paths loop
            declare
               field : constant String := specific_field (env_path, field_number, ":");
               check_path : Boolean := True;
            begin
               if field (field'First) = '/' then
                  declare
                     first_dir : constant String := specific_field (field, 2, "/");
                  begin
                     if first_dir = "usr" or else
                       first_dir = "home" or else
                       first_dir = "sbin" or else
                       first_dir = "bin" or else
                       first_dir = "lib" or else
                       first_dir = "libexec"
                     then
                        check_path := False;
                     end if;
                  end;
               else
                  check_path := False;
               end if;
               if check_path then
                  if Archive.Unix.file_exists (field) then
                     if field (field'Last) = '/' then
                        patterns.append (SUS (field & name_pattern));
                     else
                        patterns.append (SUS (field & '/' & name_pattern));
                     end if;
                  end if;
               end if;
            end;
         end loop;
      end;
   end add_path_patterns;

end Raven.Cmd.Which;
