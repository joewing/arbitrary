--------------------------------------------------------------------------
-- Pi calculator
-- Joe Wingbermuehle
--------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

with Arbitrary; use Arbitrary;
with Arbitrary.Const; use Arbitrary.Const;

procedure Pi is
  precision   : Integer;
  result      : Arbitrary_Type;
begin

  if Argument_Count /= 1 then
    Put_Line ("usage: " & Command_Name & " <digits>");
    return;
  end if;

  precision := Integer'Value (Argument (1));

  result := Pi (precision);
  Put_Line (to_str (result));

end Pi;
