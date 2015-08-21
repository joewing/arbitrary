--------------------------------------------------------------------------
-- Arbitrary Precision Math Library: Logarithmic Functions
-- Joe Wingbermuehle 20020320 <> 20020327
--------------------------------------------------------------------------

package Arbitrary.Log is

	function Exp(a : Arbitrary_Type) return Arbitrary_Type;

	function Ln2(precision : integer) return Arbitrary_Type;

	function Ln(a : Arbitrary_Type) return Arbitrary_Type;

end Arbitrary.Log;

