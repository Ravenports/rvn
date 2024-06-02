--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Event;
with Raven.Context;
with Raven.Strings;
with Raven.Pkgtypes;
with Raven.Database.Query;
with Raven.Database.CommonSQL;

use Raven.Strings;
use Raven.Pkgtypes;

package body Raven.Database.Remove is

   package QRY renames Raven.Database.Query;


   -------------------------------
   --  top_level_deletion_list  --
   -------------------------------
   function top_level_deletion_list
     (db             : RDB_Connection;
      packages       : in out Pkgtypes.Package_Set.Vector;
      pattern        : String;
      all_packages   : Boolean;
      override_exact : Boolean;
      force          : Boolean) return Boolean
   is
      leading_match : Boolean := False;
      success : Boolean := True;
      func    : constant String := "top_level_deletion_list";
      sqlbase : constant String := "SELECT " & nsv_formula & " as nsv000, " &
        "namebase, subpackage, variant, version, comment, desc, www, maintainer, prefix, " &
        "abi, rvndigest, rvnsize, flatsize, licenselogic, id " &
        "FROM packages as p";
      new_stmt : SQLite.thick_stmt;
      sql : Text;
   begin
      if all_packages then
         if force then
            sql := SUS (sqlbase);
         else
            sql := SUS (sqlbase & " WHERE nsv000 NOT GLOB 'rvn-*-standard'");
         end if;
      else
         if override_exact then
            sql := SUS (sqlbase & " WHERE nsv000 = ?");
         elsif Context.reveal_case_sensitive then
            sql := SUS (sqlbase & " WHERE nsv000 GLOB ?");
         else
            sql := SUS (sqlbase & " WHERE nsv000 LIKE ?");
            leading_match := True;
         end if;
      end if;

      if not SQLite.prepare_sql (db.handle, USS (sql), new_stmt) then
         Database.CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func, USS (sql));
         return False;
      end if;

      if not all_packages then
         if leading_match then
            SQLite.bind_string (new_stmt, 1, pattern & '%');
         else
            SQLite.bind_string (new_stmt, 1, pattern);
         end if;
      end if;
      debug_running_stmt (new_stmt);

      loop
         case SQLite.step (new_stmt) is
            when SQLite.no_more_data => exit;
            when SQLite.row_present =>
               declare
                  myrec : Pkgtypes.A_Package;
               begin
                  myrec.namebase   := SUS (SQLite.retrieve_string (new_stmt, 1));
                  myrec.subpackage := SUS (SQLite.retrieve_string (new_stmt, 2));
                  myrec.variant    := SUS (SQLite.retrieve_string (new_stmt, 3));
                  myrec.version    := SUS (SQLite.retrieve_string (new_stmt, 4));
                  myrec.comment    := SUS (SQLite.retrieve_string (new_stmt, 5));
                  myrec.desc       := SUS (SQLite.retrieve_string (new_stmt, 6));
                  myrec.www        := SUS (SQLite.retrieve_string (new_stmt, 7));
                  myrec.maintainer := SUS (SQLite.retrieve_string (new_stmt, 8));
                  myrec.prefix     := SUS (SQLite.retrieve_string (new_stmt, 9));
                  myrec.abi        := SUS (SQLite.retrieve_string (new_stmt, 10));
                  myrec.rvndigest  := SUS (SQLite.retrieve_string (new_stmt, 11));
                  myrec.rvnsize    := Package_Size (SQLite.retrieve_integer (new_stmt, 12));
                  myrec.flatsize   := Package_Size (SQLite.retrieve_integer (new_stmt, 13));
                  myrec.licenselogic := License_Logic'Val (SQLite.retrieve_integer (new_stmt, 14));
                  myrec.id         := Package_ID (SQLite.retrieve_integer (new_stmt, 15));

                  QRY.finish_package_scripts (db, myrec);
                  QRY.finish_package_messages (db, myrec);
                  QRY.finish_package_directories (db, myrec);
                  QRY.finish_package_files (db, myrec);
                  packages.Append (myrec);
                  Event.emit_debug (moderate, "Added to top-level match for removal: " &
                                      Pkgtypes.nsv_identifier (myrec));
               end;
            when SQLite.something_else =>
               CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                            SQLite.get_expanded_sql (new_stmt));
               success := False;
               exit;
         end case;
      end loop;
      SQLite.finalize_statement (new_stmt);
      return success;

   end top_level_deletion_list;


   -------------------------
   --  recursive_removal  --
   -------------------------
   procedure recursive_removal
     (db             : RDB_Connection;
      top_packages   : Pkgtypes.Package_Set.Vector;
      purge_list     : in out Pkgtypes.Package_Set.Vector;
      force          : Boolean)
   is
      already_seen : ID_Set.Map;
      queue        : ID_Set.Map;
      ondeck       : ID_Set.Map;
      patterns     : Pkgtypes.Text_List.Vector;

      procedure copy (Position : Pkgtypes.Package_Set.Cursor)
      is
         nsv : constant String := Pkgtypes.nsv_identifier (Pkgtypes.Package_Set.Element (Position));
         pid : constant Pkgtypes.Package_ID := Pkgtypes.Package_Set.Element (Position).id;
      begin
         purge_list.Append (Pkgtypes.Package_Set.Element (Position));
         already_seen.Insert (SUS (nsv), pid);
         queue.Insert (SUS (nsv), pid);
      end copy;

      procedure analyze (Position : ID_Set.Cursor)
      is
         nsv : Text renames ID_Set.Key (Position);
         rdeps : ID_Set.Map;

         Procedure check_rdep (innerpos : ID_Set.Cursor)
         is
            rdep_nsv : Text renames ID_Set.Key (innerpos);
         begin
            if already_seen.Contains (rdep_nsv) then
               return;
            end if;
            patterns.Append (rdep_nsv);
            ondeck.Insert (rdep_nsv, ID_Set.Element (innerpos));
            already_seen.Insert (rdep_nsv, ID_Set.Element (innerpos));
            Event.emit_debug (moderate, "remove reverse dependency: " & USS (rdep_nsv));
         end check_rdep;
      begin
         gather_reverse_dependencies (db, rdeps, nsv);
         rdeps.Iterate (check_rdep'Access);
      end analyze;

      procedure transfer_to_queue (Position : ID_Set.Cursor) is
      begin
         queue.Insert (ID_Set.Key (Position), ID_Set.Element (Position));
      end transfer_to_queue;

      procedure add_to_purge_list (Position : Pkgtypes.Text_List.Cursor)
      is
         pattern : constant String := USS (Pkgtypes.Text_List.Element (Position));
      begin
         if top_level_deletion_list (db             => db,
                                     packages       => purge_list,
                                     pattern        => pattern,
                                     all_packages   => False,
                                     override_exact => True,
                                     force          => False)
         then
            Event.emit_debug (moderate, "Failed to get reverse dep from " & pattern);
         end if;
      end add_to_purge_list;

   begin
      purge_list.Clear;
      patterns.Clear;
      ondeck.Clear;
      queue.Clear;

      top_packages.Iterate (copy'Access);
      if force then
         return;
      end if;

      loop
         exit when queue.Is_Empty;
         queue.Iterate (analyze'Access);
         queue.Clear;
         ondeck.Iterate (transfer_to_queue'Access);
         ondeck.Clear;
      end loop;

      patterns.Iterate (add_to_purge_list'Access);

   end recursive_removal;


   -----------------------------------
   --  gather_reverse_dependencies  --
   -----------------------------------
   procedure gather_reverse_dependencies
     (db             : RDB_Connection;
      rdependencies  : in out ID_Set.Map;
      target_nsv     : Text)
   is
      new_stmt : SQLite.thick_stmt;
      func : constant String := "gather_reverse_dependencies";
      sql  : constant String :=
        "SELECT p.id, p.namebase ||'-'|| p.subpackage ||'-'|| p.variant as nsv " &
        "FROM packages as p JOIN pkg_dependencies x on x.package_id = p.id " &
        "WHERE x.dependency_id = (SELECT d.dependency_id FROM dependencies d WHERE nsv = ?)";
   begin
      rdependencies.Clear;
      if not SQLite.prepare_sql (db.handle, sql, new_stmt) then
         CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func, sql);
         return;
      end if;
      SQLite.bind_string (new_stmt, 1, USS (target_nsv));
      debug_running_stmt (new_stmt);

      loop
         case SQLite.step (new_stmt) is
            when SQLite.no_more_data => exit;
            when SQLite.row_present =>
               declare
                  nsv : constant String := SQLite.retrieve_string (new_stmt, 1);
                  pid : constant Pkgtypes.Package_ID :=
                    Pkgtypes.Package_ID (SQLite.retrieve_integer (new_stmt, 0));
               begin
                  rdependencies.Insert (SUS (nsv), pid);
               end;
            when SQLite.something_else =>
               CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                            SQLite.get_expanded_sql (new_stmt));
         end case;
      end loop;
      SQLite.finalize_statement (new_stmt);
   end gather_reverse_dependencies;


end Raven.Database.Remove;
