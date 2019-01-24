
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

with Arbitrary; use Arbitrary;
with Arbitrary.Log; use Arbitrary.Log;

procedure Ln2 is
  precision    : Integer;
begin

  if Argument_Count /= 1 then
    Put_Line ("usage: " & Command_Name & " <digits>");
    return;
  end if;

  precision := Integer'Value (Argument (1));

  declare
    result    : Arbitrary_Type (precision);
  begin
    result := Ln2 (precision);
    Put_Line (to_str (result));
  end;

end Ln2;
