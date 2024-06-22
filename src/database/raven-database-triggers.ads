--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Containers.Vectors;
with Raven.Pkgtypes;


package Raven.Database.Triggers is

   type A_Directory_Trigger is
      record
         namebase : Text;
         subpkg   : Text;
         variant  : Text;
         prefix   : Text;
         script   : Text;
         ent_path : Text;
         trig_id  : Natural;
         pkg_id   : Pkgtypes.Package_ID;
      end record;

   package Directory_Triggers is new Ada.Containers.Vectors
     (Element_Type => A_Directory_Trigger,
      Index_Type   => Natural);

   --  establishes list of directories that are triggers
   procedure get_directory_triggers
     (db       : Database.RDB_Connection;
      dir_list : in out Directory_Triggers.Vector);

   --  Returns boolean on the presence of the three types of file triggers
   procedure file_triggers_present
     (db            : Database.RDB_Connection;
      pattern_exact : out Boolean;
      pattern_glob  : out Boolean;
      pattern_regex : out Boolean);

   --  Establishes list of files that trigger from exact patterns
   --  pkgid_set in format "(x,y,z)"
   procedure get_file_exact_triggers
     (db        : Database.RDB_Connection;
      pkgid_set : String;
      file_list : in out Directory_Triggers.Vector);

   --  Establishes list of files that trigger from glob patterns
   procedure get_file_glob_triggers
     (db        : Database.RDB_Connection;
      pkgid_set : String;
      file_list : in out Directory_Triggers.Vector);

   --  Establishes list of files that trigger from regular expressions
   procedure get_file_regexp_triggers
     (db        : Database.RDB_Connection;
      pkgid_set : String;
      file_list : in out Directory_Triggers.Vector);

private

   internal_srcfile : constant String := "raven-database-triggers.adb";

   procedure get_file_triggers_core
     (db        : Database.RDB_Connection;
      pkgid_set : String;
      sql_file  : String;
      sql_patt  : String;
      file_list : in out Directory_Triggers.Vector);


end Raven.Database.Triggers;
