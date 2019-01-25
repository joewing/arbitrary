--------------------------------------------------------------------------
-- Arbitrary Precision Math Library: Logarithmic Functions
-- Joe Wingbermuehle 20020320 <> 20020327
--------------------------------------------------------------------------
pragma Ada_2012;
pragma Detect_Blocking;

package Arbitrary.Log
  with preelaborate
is

  function Exp (a : Arbitrary_Type) return Arbitrary_Type;

  function Ln2 (precision : Integer) return Arbitrary_Type;

  function Ln (a : Arbitrary_Type) return Arbitrary_Type;

end Arbitrary.Log;
