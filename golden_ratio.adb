
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

with Arbitrary; use Arbitrary;
with Arbitrary.Const; use Arbitrary.Const;

procedure Golden_Ratio is
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

		result := Golden_Ratio(precision);

		Display(result);

	end;

end Golden_Ratio;

