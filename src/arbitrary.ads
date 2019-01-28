--------------------------------------------------------------------------
-- Arbitrary Precision Math Library
-- Joe Wingbermuehle 20020320 <> 20020327
--------------------------------------------------------------------------
pragma Ada_2012;
pragma Detect_Blocking;

with Ada.Finalization;

package Arbitrary
  with preelaborate
is

  -- Set to false for increased speed
  DEBUG_CHECKS  : Boolean  := False;

  type Arbitrary_Type (size : Positive) is
    new Ada.Finalization.Controlled with private;

  function to_str (a : Arbitrary_Type)  return String
    with pure_function;
  procedure Clear (a : in out Arbitrary_Type);

  function To_Arbitrary (value : Integer; precision : Integer)
    return Arbitrary_Type;

  function Factorial (n : Integer; precision : Integer) return Arbitrary_Type
    with inline;

  function One_Over_Factorial (n : Integer; precision : Integer)
    return Arbitrary_Type
      with inline;

  function Square_Root (a : Arbitrary_Type) return Arbitrary_Type;

  function "="(a, b : Arbitrary_Type) return Boolean
    with inline;
  function ">"(a, b : Arbitrary_Type) return Boolean;
  function ">="(a, b : Arbitrary_Type) return Boolean;
  function "<"(a, b : Arbitrary_Type) return Boolean;
  function "<="(a, b : Arbitrary_Type) return Boolean;

  function "+"(a : Arbitrary_Type) return Arbitrary_Type
    with inline, pure_function;
  function "-"(a : Arbitrary_Type) return Arbitrary_Type
    with inline, pure_function;

  function "+"(a, b : Arbitrary_Type) return Arbitrary_Type;
  function "-"(a, b : Arbitrary_Type) return Arbitrary_Type;
  function "*"(a, b : Arbitrary_Type) return Arbitrary_Type
    with inline;
  function "/"(a, b : Arbitrary_Type) return Arbitrary_Type
    with inline;

  function "+"(a : Arbitrary_Type; b : Integer) return Arbitrary_Type;
  function "+"(a : Integer; b : Arbitrary_Type) return Arbitrary_Type;
  function "-"(a : Arbitrary_Type; b : Integer) return Arbitrary_Type;
  function "-"(a : Integer; b : Arbitrary_Type) return Arbitrary_Type;
  function "+"(a, b : Integer) return Arbitrary_Type;
  function "-"(a, b : Integer) return Arbitrary_Type;

private

  base    : constant Integer := 10;
  fbase   : constant Float   := Float (base);

  type Mantissa_Type is array (Positive range <>) of Integer
    with  preelaborable_initialization,
          default_component_value => 0;

  type Mantissa_Pointer is access Mantissa_Type;

  type Arbitrary_Type (size : Positive) is
    new Ada.Finalization.Controlled with record
      mantissa    : Mantissa_Pointer;
      exponent    : Integer;
      sign        : Integer range -1 .. 1;
      precision   : Positive;
  end record;

  overriding
  procedure Initialize (Object : in out Arbitrary_Type);

  overriding
  procedure Adjust (Object : in out Arbitrary_Type);

  overriding
  procedure Finalize (Object : in out Arbitrary_Type);

end Arbitrary;
