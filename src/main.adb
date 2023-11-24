--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Command_Line;
with GNAT.Exception_Traces;

procedure Main is
   package ACL renames Ada.Command_Line;
begin
   GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Unhandled_Raise);

   --  TODO: Finish main
    ACL.Set_Exit_Status (Code => ACL.Success);
end Main;
