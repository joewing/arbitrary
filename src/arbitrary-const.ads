--------------------------------------------------------------------------
-- Arbitrary Precision Math Library: Constants
-- Joe Wingbermuehle 20020320 <> 20020327
--------------------------------------------------------------------------

package Arbitrary.Const
  with preelaborate
is

  function Pi (precision : Integer) return Arbitrary_Type;
  function Golden_Ratio (precision : Integer) return Arbitrary_Type;

end Arbitrary.Const;
