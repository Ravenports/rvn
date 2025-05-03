--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Strings.Unbounded;

package Raven is

   package SU renames Ada.Strings.Unbounded;
   
   progversion    : constant String := "0.4.8";
   progname       : constant String := "rvn";
   extension      : constant String := ".rvn";
   install_loc    : constant String := "/raven";

   --------------------
   --  Global Types  --
   --------------------
   subtype Text is SU.Unbounded_String;
   type A_Debug_Level is (silent, high_level, moderate, low_level);
   type IP_support is (no_restriction, IPv4_only, IPv6_only);
   type int64 is range -(2**63) .. +(2**63 - 1);

    type Action_Result is
     (RESULT_OK,         --  Nominal
      RESULT_END,        --  No more items available (end of the loop)
      RESULT_WARN,       --  The function encountered a non-fatal error
      RESULT_FATAL,      --  The function encountered a fatal error
      RESULT_ENODB,      --  Can not create local database or database non-existent
      RESULT_REQUIRED,   --  Can not delete the package because it is required by another package
      RESULT_INSTALLED,  --  Can not install the package because it is already installed.
      RESULT_DEPENDENCY, --  Can not install the package because some dependencies are unresolved
      RESULT_ENOACCESS   --  Insufficient privilege for action
     );
   
end Raven;
