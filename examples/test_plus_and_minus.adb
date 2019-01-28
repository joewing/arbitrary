pragma Ada_2012;

with Ada.Text_IO; use Ada.Text_IO;
with Arbitrary;   use Arbitrary;
with Ada.Command_Line; use Ada.Command_Line;

procedure test_plus_and_minus is
  precision    : Integer;
  val1         : Integer;
  val2         : Integer;
begin

  if Argument_Count /= 3 then
    Put_Line ("usage: " & Command_Name & " <digits> <val1> <val2>");
    return;
  end if;

  precision := Integer'Value (Argument (1));
  val1      := Integer'Value (Argument (2));
  val2      := Integer'Value (Argument (3));

  Put_Line (to_str (To_Arbitrary (val1, precision) + val2));
  Put_Line (to_str (val1 + To_Arbitrary (val2, precision)));
  Put_Line (to_str (To_Arbitrary (val1, precision) - val2));
  Put_Line (to_str (val1 - To_Arbitrary (val2, precision)));
  Put_Line (to_str (val1 + val2));
  Put_Line (to_str (val2 + val1));
  Put_Line (to_str (val1 - val2));
  Put_Line (to_str (val2 - val1));

end test_plus_and_minus;
