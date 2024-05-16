--  SPDX-License-Identifier: ISC
--  Reference: /License.txt


with Raven.Event;

package body Raven.Cmd.Clean is

   -----------------------------
   --  execute_clean_command  --
   -----------------------------
   function execute_clean_command (comline : Cldata) return Boolean
   is
   begin
      Event.emit_notice ("Clean doesn't do anything yet");
      return True;
   end execute_clean_command;

end Raven.Cmd.Clean;
