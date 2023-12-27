--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

private with Raven.Database;
private with Raven.Pkgtypes;

package Raven.Cmd.Which is

   --  Executes which command
   function execute_which_command (comline : Cldata) return Boolean;

private

   rdb : Database.RDB_Connection;

   --  concatent directories in PATH with the name pattern
   --  exclude directories: /bin, /sbin, /usr, /home since rvn will never install there
   procedure add_path_patterns (name_pattern : String; patterns : in out Pkgtypes.Text_List.Vector);

end Raven.Cmd.Which;
