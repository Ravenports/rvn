--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Raven.Strings;
use Raven.Strings;

package body Raven.Pkgtypes is


   -----------------------
   --  nsvv_identifier  --
   -----------------------
   function nsvv_identifier (pkg : A_Package) return String
   is
      N : constant String := USS (pkg.namebase);
      S : constant String := USS (pkg.subpackage);
      V : constant String := USS (pkg.variant);
      Z : constant String := USS (pkg.version);
   begin
      return N & '-' & S & '-' & V & '-' & Z;
   end nsvv_identifier;

end Raven.Pkgtypes;
