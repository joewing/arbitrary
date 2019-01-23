
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

with Arbitrary; use Arbitrary;
with Arbitrary.Log; use Arbitrary.Log;

procedure E is
	precision		: integer;
begin

	if Argument_Count /= 1 then
		Put_Line("usage: " & Command_Name & " <digits>");
		return;
	end if;

	precision := integer'value(Argument(1));

	declare
		result		: Arbitrary_Type(precision);
	begin

		result := Exp(To_Arbitrary(1, precision));

		Put_line (to_str (result));

	end;

end E;
