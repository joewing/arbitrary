--------------------------------------------------------------------------
-- Arbitrary Precision Math Library: Constants
-- Joe Wingbermuehle 20020320 <> 20020327
--------------------------------------------------------------------------
pragma Ada_2012;
pragma Detect_Blocking;

package Arbitrary.Const is

  function Pi (precision : Integer) return Arbitrary_Type;
  function Golden_Ratio (precision : Integer) return Arbitrary_Type;

end Arbitrary.Const;
