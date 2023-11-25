--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Command_Line;
with Raven.Cmd.Line;
with Raven.Cmd.Usage;
with GNAT.Exception_Traces;

procedure Main is
   package CLI renames Ada.Command_Line;

   comline_inputs : Raven.Cmd.Cldata;
begin
   GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Unhandled_Raise);

   comline_inputs := Raven.Cmd.Line.parse_command_line;
   if not Raven.Cmd.Usage.command_line_valid (comline_inputs) then
      CLI.Set_Exit_Status (Code => CLI.Failure);
      return;
   end if;
   CLI.Set_Exit_Status (Code => CLI.Success);
end Main;
