--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Interfaces.C.Strings;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Text_IO;
with Raven.Unix;
with Blake_3;

--  The purpose of this tool is to accept a 64-character representation of a Blake3
--  hash and sign the 32-bit binary version of that with an RSA key pair located
--  at ../etc/ravensign directory.  The standard output looks like this:
--
--  SIGNATURE
--  {256-byte binary signature}
--  CERT
--  {Copy of public key}
--  END
--
--  If the first argument is invalid (not 64 characters, not hex characters), or if
--  the RSA keypair is missing, the following will be output:
--
--  FAILURE
--  {reason for failure}
--
--  The key files must be named "rvnrepo.key" and "rvnrepo.pub" and be located in the
--  relative directory ../etc/ravensign.  The "ravensign" tool itself should be located
--  in a standard path such as /raven/bin, /usr/local/bin, /opt/bin, etc.
--  The usual method to invoke the server is with SSH, e.g.
--  "ssh <hosting-server> ravensign <Blake3-hex-hash>"
--

procedure ravensign
is
   package CLI renames Ada.Command_Line;
   package DIR renames Ada.Directories;
   package TIO renames Ada.Text_IO;
   package IC  renames Interfaces.C;

   procedure quit (message : String);
   procedure quit (message : String) is
   begin
      TIO.Put_Line ("FAILURE");
      TIO.Put_Line (message);
   end quit;

   key_path    : constant String := DIR.Current_Directory & "/../etc/ravensign/rvnrepo.key";
   pubkey_path : constant String := DIR.Current_Directory & "/../etc/ravensign/rvnrepo.pub";

   binary_hash : Blake_3.blake3_hash;
   c_key_path  : IC.Strings.chars_ptr;
   c_hash_len  : IC.size_t := IC.size_t (Blake_3.blake3_hash'Length);
   c_hash      : array (Blake_3.blake3_hash'Range) of aliased IC.unsigned_char;
   c_capacity  : IC.size_t := 1024;
   c_signature : array (1 .. c_capacity) of aliased IC.unsigned_char := (others => 0);
   c_sig_len   : aliased IC.size_t := 0;
   result      : IC.int;

begin
   if CLI.Argument_Count = 0 then
      quit ("The first argument, the Blake3 hash, is missing.");
      return;
   end if;

   declare
      catalog_hash : constant String := CLI.Argument (1);
   begin
      if catalog_hash'Length /= 64 then
         quit ("The Blake3 hash is not the expected 64 characters in length.");
         return;
      end if;
      begin
         binary_hash := Blake_3.reverse_hex (catalog_hash);
      exception
         when Blake_3.invalid_hex_hash =>
            quit ("The Blake3 hash contains invalid characters (0-9, A-F, a-f, only)");
            return;
      end;
   end;

   if not DIR.Exists (key_path) then
      quit ("The private key is not installed.");
      return;
   end if;

   if not DIR.Exists (pubkey_path) then
      quit ("The public key is not installed.");
      return;
   end if;

   case DIR.Kind (pubkey_path) is
      when DIR.Ordinary_File => null;
      when others =>
         quit ("The public is not a regular file.");
         return;
   end case;

   c_key_path := IC.Strings.New_String (key_path);

   for x in Blake_3.blake3_hash'Range loop
      c_hash (x) := IC.unsigned_char (Character'Pos (binary_hash (x)));
   end loop;

   result := Raven.Unix.C_Sign_Digest
        (hash      => c_hash (Blake_3.blake3_hash'First)'Access,
         hash_len  => c_hash_len,
         key_path  => c_key_path,
         signature => c_signature (1)'Access,
         sig_cap   => c_capacity,
         sig_len   => c_sig_len'Access);

   IC.Strings.Free (c_key_path);

   case result is
      when 0 => null;
      when others =>
         quit ("Failed to calculate signature.");
         return;
   end case;

   TIO.Put_Line ("SIGNATURE");
   for z in 1 .. c_sig_len loop
      TIO.Put (Character'Val (c_signature (z)));
   end loop;
   TIO.Put_Line ("");
   TIO.Put_Line ("CERT");
   declare
      file_handle : TIO.File_Type;
   begin
      TIO.Open (File => file_handle, Mode => TIO.In_File, Name => pubkey_path);
      While not TIO.End_Of_File (file_handle) Loop
         TIO.Put_Line (TIO.Get_Line (file_handle));
      end loop;
      TIO.Close (file_handle);
   exception
      when others =>
         if TIO.Is_Open (file_handle) then
            TIO.Close (file_handle);
         end if;
   end;
   TIO.Put_Line ("END");

end ravensign;
