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
   
   progversion    : constant String := "0.0.1";
   progname       : constant String := "rvn";
   platform       : constant Operating_System := Operating_System'First;   
   db_schema_ver  : constant Natural := 1;
   local_rvn_db   : constant String := "local.sqlite";

   --------------------
   --  Global Types  --
   --------------------
   subtype Text is SU.Unbounded_String;
   subtype A_Debug_Level is Natural range 0 .. 3;
   type int64 is range -(2**63) .. +(2**63 - 1);

   
end Raven;
