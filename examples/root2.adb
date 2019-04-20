
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

with Arbitrary; use Arbitrary;

procedure Root2 is
  precision   : Integer;
  result      : Arbitrary_Type;
begin

  if Argument_Count /= 1 then
    Put_Line ("usage: " & Command_Name & " <digits>");
    return;
  end if;

  precision := Integer'Value (Argument (1));

  result := Square_Root (To_Arbitrary (2, precision));
  Put_Line (to_str (result));

end Root2;
