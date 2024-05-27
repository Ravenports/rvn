--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Event;
with Raven.Database.CommonSQL;
with Raven.Strings;  use Raven.Strings;
with Raven.Pkgtypes; use Raven.Pkgtypes;

package body Raven.Database.Fetch is


   -------------------------------------
   --  retrieve_remote_file_metadata  --
   -------------------------------------
   procedure retrieve_remote_file_metadata
     (db           : RDB_Connection;
      sql          : String;
      bind_one     : String;
      like_match   : Boolean;
      remote_files : in out Remote_Files_Set.Map)
   is
      func     : constant String := "retrieve_remote_file_metadata";
      new_stmt : SQLite.thick_stmt;
   begin
      if not SQLite.prepare_sql (db.handle, sql, new_stmt) then
         CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func, sql);
         return;
      end if;
      if bind_one /= "" then
         if like_match then
            SQLite.bind_string (new_stmt, 1, bind_one);
         else
            SQLite.bind_string (new_stmt, 1, bind_one & '%');
         end if;
      end if;
      debug_running_stmt (new_stmt);


      loop
         case SQLite.step (new_stmt) is
            when SQLite.row_present =>
               declare
                  myrec : A_Remote_File;
                  b3dig : constant String := SQLite.retrieve_string (new_stmt, 3);
                  key   : constant short_digest := b3dig (b3dig'First .. b3dig'First + 9);
               begin
                  if not remote_files.Contains (key) then
                     myrec.nsvv := SUS (SQLite.retrieve_string (new_stmt, 0));
                     myrec.flatsize := Package_Size (SQLite.retrieve_integer (new_stmt, 1));
                     myrec.rvnsize  := Package_Size (SQLite.retrieve_integer (new_stmt, 2));
                     remote_files.Insert (key, myrec);
                  end if;
               end;
            when SQLite.something_else =>
               CommonSQL.ERROR_STMT_SQLITE (db.handle, internal_srcfile, func,
                                            SQLite.get_expanded_sql (new_stmt));
               exit;
            when SQLite.no_more_data => exit;
         end case;
      end loop;
      SQLite.finalize_statement (new_stmt);

   end retrieve_remote_file_metadata;


   --------------------------
   --  rvn_core_retrieval  --
   --------------------------
   function rvn_core_retrieval
     (db           : RDB_Connection;
      patterns     : Pkgtypes.Text_List.Vector;
      behave_exact : Boolean;
      behave_cs    : Boolean;
      behave_quiet : Boolean;
      behave_yes   : Boolean;
      select_all   : Boolean;
      select_deps  : Boolean;
      destination  : String) return Boolean
   is
      basesql : constant String :=
        "SELECT namebase ||-|| subpackage ||-|| variant ||-|| version as nsvv, " &
        "flatsize, rvnsize, rvndigest FROM packages";
      download_list : Remote_Files_Set.Map;
      num_patterns  : constant Natural := Natural (patterns.Length);
      leading_match : constant Boolean := not behave_exact and then not behave_cs;

      function extended_sql return String is
      begin
         if behave_exact then
            return basesql & " AND nsvv = ?";
         elsif behave_cs then
            return basesql &" AND nsvv GLOB ?";
         end if;
         return basesql & " AND nsvv LIKE ?";
      end extended_sql;
   begin
      if select_deps then
         Event.emit_error ("--dependencies not yet supported");
         return False;
      end if;

      if select_all then
        retrieve_remote_file_metadata (db, basesql, "", False, download_list);
      else
         for pattx in 0 .. num_patterns - 1 loop
            retrieve_remote_file_metadata
              (db           => db,
               sql          => extended_sql,
               bind_one     => USS (patterns.Element (pattx)),
               like_match   => leading_match,
               remote_files => download_list);
         end loop;
      end if;
      return False;
   end rvn_core_retrieval;

end Raven.Database.Fetch;
