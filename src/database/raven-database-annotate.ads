--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Pkgtypes;

package Raven.Database.Annotate is

   procedure annotate_packages
     (db       : in out RDB_Connection;
      tag      : String;
      note     : String;
      packages : Pkgtypes.Package_Set.Vector);

   procedure remove_annotations
     (db       : RDB_Connection;
      tag      : String;
      packages : Pkgtypes.Package_Set.Vector);

private

   internal_srcfile : constant String := "raven-database-annotate.adb";

   delsql : constant String :=
     "DELETE FROM pkg_annotations " &
     "WHERE package_id = ? and annotation_id = " &
     "(SELECT annotation_id FROM annotations WHERE note_key = ?)";

   procedure commit_or_rollback
     (db        : RDB_Connection;
      revert    : Boolean;
      func      : String;
      savepoint : String);

   procedure remove_annotations_core
     (db       : RDB_Connection;
      tag      : String;
      packages : Pkgtypes.Package_Set.Vector;
      del_stmt : in out SQLite.thick_stmt);

   procedure annotate_packages_core
     (db       : in out RDB_Connection;
      tag      : String;
      note     : String;
      packages : Pkgtypes.Package_Set.Vector;
      del_stmt : in out SQLite.thick_stmt);

end Raven.Database.Annotate;
