
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

with Arbitrary; use Arbitrary;

procedure Root2 is

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
		result := Square_Root(To_Arbitrary(2, precision));
		Display(result);
	end;

end Root2;

