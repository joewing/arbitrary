--------------------------------------------------------------------------
-- Arbitrary Precision Math Library: Trigonometric Functions
-- Joe Wingbermuehe 20020320 <> 20020327
--------------------------------------------------------------------------

pragma Ada_2012;
pragma Detect_Blocking;

package Arbitrary.Trig is

  function Sin (a : Arbitrary_Type) return Arbitrary_Type;
  function Cos (a : Arbitrary_Type) return Arbitrary_Type;
  function Tan (a : Arbitrary_Type) return Arbitrary_Type;
  function Csc (a : Arbitrary_Type) return Arbitrary_Type;
  function Sec (a : Arbitrary_Type) return Arbitrary_Type;
  function Cot (a : Arbitrary_Type) return Arbitrary_Type;

  function ArcSin (a : Arbitrary_Type) return Arbitrary_Type;
  function ArcCos (a : Arbitrary_Type) return Arbitrary_Type;
  function ArcTan (a : Arbitrary_Type) return Arbitrary_Type;
  function ArcCsc (a : Arbitrary_Type) return Arbitrary_Type;
  function ArcSec (a : Arbitrary_Type) return Arbitrary_Type;
  function ArcCot (a : Arbitrary_Type) return Arbitrary_Type;

  function Sinh (a : Arbitrary_Type) return Arbitrary_Type;
  function Cosh (a : Arbitrary_Type) return Arbitrary_Type;
  function Tanh (a : Arbitrary_Type) return Arbitrary_Type;
  function Csch (a : Arbitrary_Type) return Arbitrary_Type;
  function Sech (a : Arbitrary_Type) return Arbitrary_Type;
  function Coth (a : Arbitrary_Type) return Arbitrary_Type;

  function ArcSinh (a : Arbitrary_Type) return Arbitrary_Type;
  function ArcCosh (a : Arbitrary_Type) return Arbitrary_Type;
  function ArcTanh (a : Arbitrary_Type) return Arbitrary_Type;
  function ArcCsch (a : Arbitrary_Type) return Arbitrary_Type;
  function ArcSech (a : Arbitrary_Type) return Arbitrary_Type;
  function ArcCoth (a : Arbitrary_Type) return Arbitrary_Type;

end Arbitrary.Trig;
