--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Command_Line;
with Raven.Cmd.Line;
with Raven.Cmd.Usage;
with Raven.Cmd.Bahnhof;
with Raven.Cmd.Unset;
with GNAT.Exception_Traces;

procedure Rvn is
   package CLI renames Ada.Command_Line;
   package RCU renames Raven.Cmd.Usage;

   comline_inputs : Raven.Cmd.Cldata;
begin
   GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Unhandled_Raise);

   comline_inputs := Raven.Cmd.Line.parse_command_line;
   case RCU.precheck_command_line (comline_inputs) is
      when RCU.error_found | RCU.nothing_to_do =>
         CLI.Set_Exit_Status (Code => CLI.Failure);
         return;
      when RCU.action_needed =>
         null;
      when RCU.command_pending =>
         Raven.Cmd.Unset.initialize_program (comline_inputs);
         if not Raven.Cmd.Line.pending_command_recognized (comline_inputs) then
            RCU.alert_command_unrecognized (comline_inputs);
            CLI.Set_Exit_Status (Code => CLI.Failure);
            return;
         end if;
         Raven.Cmd.Line.parse_secondary_command(comline_inputs);
         if not Raven.Cmd.Usage.command_line_valid (comline_inputs) then
            CLI.Set_Exit_Status (Code => CLI.Failure);
            return;
         end if;
   end case;
   if Raven.Cmd.Bahnhof.execute_command (comline_inputs) then
      if not Raven.Cmd.Unset.exit_status_already_set then
         CLI.Set_Exit_Status (Code => CLI.Success);
      end if;
   else
      CLI.Set_Exit_Status (Code => CLI.Failure);
   end if;
end Rvn;
