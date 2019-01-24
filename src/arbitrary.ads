--------------------------------------------------------------------------
-- Arbitrary Precision Math Library
-- Joe Wingbermuehle 20020320 <> 20020327
--------------------------------------------------------------------------
pragma Ada_2012;
pragma Detect_Blocking;

with Ada.Finalization;

package Arbitrary is

  -- Set to false for increased speed
  DEBUG_CHECKS  : Boolean  := False;

  type Arbitrary_Type (size : Positive) is
    new Ada.Finalization.Controlled with private;

  function to_str (a : Arbitrary_Type)  return String;
  procedure Clear (a : out Arbitrary_Type);

  function To_Arbitrary (value : Integer; precision : Integer)
    return Arbitrary_Type;

  function Factorial (n : Integer; precision : Integer)
    return Arbitrary_Type;

  function One_Over_Factorial (n : Integer; precision : Integer)
    return Arbitrary_Type;

  function Square_Root (a : Arbitrary_Type) return Arbitrary_Type;

  function "="(a, b : Arbitrary_Type) return Boolean;
  function ">"(a, b : Arbitrary_Type) return Boolean;
  function ">="(a, b : Arbitrary_Type) return Boolean;
  function "<"(a, b : Arbitrary_Type) return Boolean;
  function "<="(a, b : Arbitrary_Type) return Boolean;

  function "+"(a : Arbitrary_Type) return Arbitrary_Type;
  function "-"(a : Arbitrary_Type) return Arbitrary_Type;

  function "+"(a, b : Arbitrary_Type) return Arbitrary_Type;
  function "-"(a, b : Arbitrary_Type) return Arbitrary_Type;
  function "*"(a, b : Arbitrary_Type) return Arbitrary_Type;
  function "/"(a, b : Arbitrary_Type) return Arbitrary_Type;

private

  base    : constant Integer := 10;
  fbase   : constant Float   := Float (base);

  type Mantissa_Type is array (Positive range <>) of Integer;
  type Mantissa_Pointer is access Mantissa_Type;

  type Arbitrary_Type (size : Positive) is
    new Ada.Finalization.Controlled with record
      mantissa    : Mantissa_Pointer;
      exponent    : Integer;
      sign        : Integer range -1 .. 1;
      precision   : Positive := size;
  end record;

  procedure Initialize (Object : in out Arbitrary_Type);
  procedure Adjust (Object : in out Arbitrary_Type);
  procedure Finalize (Object : in out Arbitrary_Type);

end Arbitrary;
