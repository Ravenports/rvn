--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Strings.Unbounded;

package Raven is

   package SU renames Ada.Strings.Unbounded;

   type Operating_System is (generic_unix,
                             freebsd,
                             dragonfly,
                             netbsd,
                             openbsd,
                             linux,
                             solaris,
                             omnios);
   
   progversion    : constant String := "0.0.3";
   progname       : constant String := "rvn";
   extension      : constant String := ".rvn";
   platform       : constant Operating_System := Operating_System'First;
   db_schema_ver  : constant Natural := 1;
   local_rvn_db   : constant String := "local.sqlite";
   install_loc    : constant String := "/raven";

   --------------------
   --  Global Types  --
   --------------------
   subtype Text is SU.Unbounded_String;
   type A_Debug_Level is (silent, high_level, moderate, low_level);
   type int64 is range -(2**63) .. +(2**63 - 1);

   
end Raven;
