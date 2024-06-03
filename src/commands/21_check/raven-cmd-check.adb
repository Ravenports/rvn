--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Event;
with Raven.Database.Query;
with Raven.Database.Operations;
with Raven.Strings;
with Archive.Unpack;
with Archive.Unix;
with Blake_3;

use Raven.Strings;

package body Raven.Cmd.Check is

   package QRY renames Raven.Database.Query;
   package OPS renames Raven.Database.Operations;
   package ARC renames Archive.Unpack;

   -----------------------------
   --  execute_check_command  --
   -----------------------------
   function execute_check_command (comline : Cldata) return Boolean
   is
      rdb : Database.RDB_Connection;
   begin
      case OPS.rdb_open_localdb (rdb, Database.installed_packages) is
         when RESULT_OK => null;
         when others => return False;
      end case;

      if not comline.cmd_check.only_files then
         check_dependencies (rdb, comline.common_options.quiet, comline.common_options.verbose);
      end if;

      if not comline.cmd_check.only_depends then
         if Archive.Unix.user_is_root then
            check_files (rdb, comline.common_options.quiet, comline.common_options.verbose);
         else
            if not comline.common_options.quiet then
               Event.emit_message ("File integrity checking is restricted to the superuser.");
            end if;
         end if;
      end if;

      OPS.rdb_close (rdb);
      return True;

   end execute_check_command;


   --------------------------
   --  check_dependencies  --
   --------------------------
   procedure check_dependencies (db : Database.RDB_Connection; quiet : Boolean; verbose : Boolean)
   is
      pkgmap  : Pkgtypes.NV_Pairs.Map;
      missing_deps : Pkgtypes.NV_Pairs.Map;
      delim : constant String := "|";

      procedure check_single_package (Position : Pkgtypes.NV_Pairs.Cursor)
      is
         nsv     : Text renames Pkgtypes.NV_Pairs.Key (Position);
         pid_str : constant String := USS (Pkgtypes.NV_Pairs.Element (Position));
         pid     : Pkgtypes.Package_ID := Pkgtypes.Package_ID (Natural'Value (pid_str));
         deps    : Pkgtypes.Text_List.Vector;

         procedure augment (key : Text; element : in out Text) is
         begin
            SU.Append (element, delim);
            SU.Append (element, nsv);
         end augment;

         procedure scan (innerpos : Pkgtypes.Text_List.Cursor)
         is
            nsv_dep : Text renames Pkgtypes.Text_List.Element (innerpos);
         begin
            if not pkgmap.Contains (nsv_dep) then
               if missing_deps.Contains (nsv_dep) then
                  missing_deps.Update_Element (missing_deps.Find (nsv_dep), augment'Access);
               else
                  missing_deps.Insert (nsv_dep, nsv);
               end if;
            end if;
         end scan;
      begin
         QRY.get_package_dependencies (db, pid, deps);
         deps.Iterate (scan'Access);
      end check_single_package;

      procedure print (Position : Pkgtypes.NV_Pairs.Cursor)
      is
         nsv : constant String := USS (Pkgtypes.NV_Pairs.Key (Position));
         affected : constant String := USS (Pkgtypes.NV_Pairs.Element (Position));

         num_affected : Natural;
      begin
         Event.emit_message (nsv);
         if verbose then
            num_affected := count_char (affected, delim (delim'First)) + 1;
            for x in 1 .. num_affected loop
               Event.emit_message ("    impacts: " & specific_field (affected, x, delim));
            end loop;
         end if;
      end print;
   begin
      QRY.all_installed_packages (db, pkgmap);
      pkgmap.Iterate (check_single_package'Access);
      if not missing_deps.Is_Empty then
         if not quiet then
            Event.emit_message ("-------------------------------");
            Event.emit_message ("  Missing dependency detected");
            Event.emit_message ("-------------------------------");
         end if;
         missing_deps.Iterate (print'Access);
         if not quiet then
            Event.emit_message ("");
         end if;
      end if;
   end check_dependencies;



   -------------------
   --  check_files  --
   -------------------
   procedure check_files (db : Database.RDB_Connection; quiet : Boolean; verbose : Boolean)
   is
      pkgmap       : Pkgtypes.NV_Pairs.Map;
      damaged_pkgs : Pkgtypes.NV_Pairs.Map;
      delim        : constant String := "|";
      null_digest  : Blake_3.blake3_hash_hex := (others => '0');


      procedure check_single_package (Position : Pkgtypes.NV_Pairs.Cursor)
      is
         nsv      : Text renames Pkgtypes.NV_Pairs.Key (Position);
         pid_str  : constant String := USS (Pkgtypes.NV_Pairs.Element (Position));
         pid      : Pkgtypes.Package_ID := Pkgtypes.Package_ID (Natural'Value (pid_str));
         pkgfiles : ARC.file_records.Vector;

         procedure scan (innerpos : ARC.file_records.Cursor)
         is
            myrec : ARC.file_record renames ARC.file_records.Element (innerpos);

            procedure augment (key : Text; element : in out Text) is
            begin
               SU.Append (element, delim);
               SU.Append (element, myrec.path);
            end augment;

            procedure augment_missing (key : Text; element : in out Text) is
            begin
               SU.Append (element, delim);
               SU.Append (element, myrec.path);
               SU.Append (element, " [missing]");
            end augment_missing;
         begin
            if Archive.Unix.file_exists (USS (myrec.path)) then
               if myrec.digest = null_digest then
                  return;
               end if;
               declare
                  sum : constant Blake_3.blake3_hash_hex :=
                    Blake_3.hex (Blake_3.file_digest (USS (myrec.path)));
               begin
                  if sum /= myrec.digest then
                     if damaged_pkgs.Contains (nsv) then
                        damaged_pkgs.Update_Element (damaged_pkgs.Find (nsv), augment'Access);
                     else
                        damaged_pkgs.Insert (nsv, myrec.path);
                     end if;
                  end if;
               end;
            else
               if damaged_pkgs.Contains (nsv) then
                  damaged_pkgs.Update_Element (damaged_pkgs.Find (nsv), augment_missing'Access);
               else
                  damaged_pkgs.Insert (nsv, SUS (USS (myrec.path) & " [missing]"));
               end if;
            end if;
         end scan;
      begin
         QRY.get_package_files (db, pid, pkgfiles);
         pkgfiles.Iterate (scan'Access);
      end check_single_package;

      procedure print (Position : Pkgtypes.NV_Pairs.Cursor)
      is
         nsv : constant String := USS (Pkgtypes.NV_Pairs.Key (Position));
         damaged_files : constant String := USS (Pkgtypes.NV_Pairs.Element (Position));

         num_affected : Natural;
      begin
         Event.emit_message (nsv);
         if verbose then
            num_affected := count_char (damaged_files, delim (delim'First)) + 1;
            for x in 1 .. num_affected loop
               Event.emit_message ("    corrupt: " & specific_field (damaged_files, x, delim));
            end loop;
         end if;
      end print;
   begin
      QRY.all_installed_packages (db, pkgmap);
      pkgmap.Iterate (check_single_package'Access);
      if not damaged_pkgs.Is_Empty then
         if not quiet then
            Event.emit_message ("----------------------------");
            Event.emit_message ("  Corrupted files detected");
            Event.emit_message ("----------------------------");
         end if;
         damaged_pkgs.Iterate (print'Access);
      end if;
   end check_files;


end Raven.Cmd.Check;
