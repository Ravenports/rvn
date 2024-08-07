--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Characters.Latin_1;
with Archive.Unix;
with Raven.Version;
with Raven.Strings;
use Raven.Strings;

package body Raven.Pkgtypes is

   package LAT renames Ada.Characters.Latin_1;

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
      return N & LAT.Tilde & S & LAT.Tilde & V & LAT.Tilde & Z;
   end nsvv_identifier;


   ----------------------
   --  nsv_identifier  --
   ----------------------
   function nsv_identifier (pkg : A_Package) return String
   is
      N : constant String := USS (pkg.namebase);
      S : constant String := USS (pkg.subpackage);
      V : constant String := USS (pkg.variant);
   begin
      return N & LAT.Tilde & S & LAT.Tilde & V;
   end nsv_identifier;


   -------------------------
   --  combined_messages  --
   -------------------------
   function combined_messages
     (pkg         : A_Package;
      mtype       : Message_Type;
      old_version : String) return String
   is
      combined_msg : Text := SU.Null_Unbounded_String;

      procedure join (Position : Message_List.Cursor)
      is
         minver : constant String := USS (Message_List.Element (Position).minimum_version);
         maxver : constant String := USS (Message_List.Element (Position).maximum_version);
         msg    : constant Text := Message_List.Element (Position).message;

         procedure add_linefeed is
         begin
            if not IsBlank (combined_msg) then
               SU.Append (combined_msg, Character'Val (10));
            end if;
         end add_linefeed;
      begin

         case mtype is
            when upgrade =>
               if minver /= "" then
                  case Version.pkg_version_cmp (old_version, minver) is
                     when -1 => return;  -- old_version < minver, eject
                     when 0 | 1 => null;   -- pkg.version >= minver, fallthrough
                  end case;
               end if;
               if maxver /= "" then      --  satified min version (might have been blank)
                  case Version.pkg_version_cmp (old_version, maxver) is
                     when -1 | 0 => null;   --  less than or equal to max version, show it.
                     when 1 => return;      --  old_version > max version, eject
                  end case;
               end if;
               add_linefeed;
               SU.Append (combined_msg, msg);

            when install | deinstall =>
               add_linefeed;
               SU.Append (combined_msg, msg);
         end case;
      end join;
   begin
      pkg.messages (mtype).Iterate (join'Access);
      return USS (combined_msg);
   end combined_messages;


   ---------------------
   --  get_file_size  --
   ---------------------
   function get_file_size (path : String) return Package_Size
   is
      features : Archive.Unix.File_Characteristics;
   begin
      features := Archive.Unix.get_charactistics (path);
      case features.ftype is
         when Archive.unsupported => return 0;
         when Archive.directory |
              Archive.symlink |
              Archive.fifo => return 0;
         when Archive.regular | Archive.hardlink => return Package_Size (features.size);
      end case;
   end get_file_size;


end Raven.Pkgtypes;
