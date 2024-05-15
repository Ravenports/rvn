--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Event;
with Raven.Strings;
with Raven.Pkgtypes;
with Raven.Database.Lock;
with Raven.Database.Query;
with Raven.Database.Operations;

use Raven.Strings;

package body Raven.Cmd.Shlib is

   package LOK renames Raven.Database.Lock;
   package QRY renames Raven.Database.Query;
   package OPS renames Raven.Database.Operations;

   -----------------------------
   --  execute_shlib_command  --
   -----------------------------
   function execute_shlib_command (comline : Cldata) return Boolean
   is
      --  guaranteed that --provides and --requires are not both set.
      --  This if --requires is not set, act as if --provides was (in case both are unset)
      library_soname : constant String := USS (comline.common_options.name_pattern);
      return_result  : Boolean;
   begin
      case OPS.rdb_open_localdb (rdb, Database.installed_packages) is
         when RESULT_OK => null;
         when others => return False;
      end case;
      if not LOK.obtain_lock (rdb, LOK.lock_readonly) then
         OPS.rdb_close (rdb);
         Event.emit_error ("Cannot get a read lock on a database, it is locked by another process");
         return False;
      end if;

      if comline.cmd_shlib.requires then
         return_result := show_requirements (comline.common_options.quiet, library_soname);
      else
         return_result := show_provisions (comline.common_options.quiet, library_soname);
      end if;

      if not LOK.release_lock (rdb, LOK.lock_readonly) then
         null;
      end if;
      OPS.rdb_close (rdb);

      return return_result;
   end execute_shlib_command;


   -----------------------
   --  show_provisions  --
   -----------------------
   function show_provisions (quiet : Boolean; library_soname : String) return Boolean
   is
      packages : Pkgtypes.Package_Set.Vector;

      procedure print (Position : Pkgtypes.Package_Set.Cursor)
      is
         this_pkg : Pkgtypes.A_Package renames Pkgtypes.Package_Set.Element (Position);
      begin
         Event.emit_message (Pkgtypes.nsvv_identifier (this_pkg));
      end print;
   begin
      QRY.provides_library (rdb, library_soname, packages);
      if packages.Is_Empty then
         if not quiet then
            Event.emit_message ("No packages provides " & library_soname & ".");
         end if;
         return False;
      end if;

      if not quiet then
         Event.emit_message (library_soname & " is provided by the following packages:");
      end if;
      packages.Iterate (print'Access);
      return True;
   end show_provisions;


   -------------------------
   --  show_requirements  --
   -------------------------
   function show_requirements (quiet : Boolean; library_soname : String) return Boolean
   is
      packages : Pkgtypes.Package_Set.Vector;

      procedure print (Position : Pkgtypes.Package_Set.Cursor)
      is
         this_pkg : Pkgtypes.A_Package renames Pkgtypes.Package_Set.Element (Position);
      begin
         Event.emit_message (Pkgtypes.nsvv_identifier (this_pkg));
      end print;
   begin
      QRY.requires_library (rdb, library_soname, packages);
      if packages.Is_Empty then
         if not quiet then
            Event.emit_message ("No packages require " & library_soname & ".");
         end if;
         return False;
      end if;

      if not quiet then
         Event.emit_message (library_soname & " is linked to the following packages:");
      end if;
      packages.Iterate (print'Access);
      return True;
   end show_requirements;


end Raven.Cmd.Shlib;
