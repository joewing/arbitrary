--------------------------------------------------------------------------
-- Arbitrary Precision Math Library
-- Joe Wingbermuehle 20020320 <> 20020327
--------------------------------------------------------------------------

with Ada.Finalization;

package Arbitrary is

	-- Set to false for increased speed
	DEBUG_CHECKS	: boolean	:= false;

	type Arbitrary_Type(size : positive) is
		new Ada.Finalization.Controlled with private;

	procedure Display(a : Arbitrary_Type);
	procedure Clear(a : out Arbitrary_Type);

	function To_Arbitrary(value : integer; precision : integer)
		return Arbitrary_Type;


	function Factorial(n : integer; precision : integer)
		return Arbitrary_Type;

	function One_Over_Factorial(n : integer; precision : integer)
		return Arbitrary_Type;

	function Square_Root(a : Arbitrary_Type) return Arbitrary_Type;

	function "="(a, b : Arbitrary_Type) return boolean;
	function ">"(a, b : Arbitrary_Type) return boolean;
	function ">="(a, b : Arbitrary_Type) return boolean;
	function "<"(a, b : Arbitrary_Type) return boolean;
	function "<="(a, b : Arbitrary_Type) return boolean;

	function "+"(a : Arbitrary_Type) return Arbitrary_Type;
	function "-"(a : Arbitrary_Type) return Arbitrary_Type;

	function "+"(a, b : Arbitrary_Type) return Arbitrary_Type;
	function "-"(a, b : Arbitrary_Type) return Arbitrary_Type;
	function "*"(a, b : Arbitrary_Type) return Arbitrary_Type;
	function "/"(a, b : Arbitrary_Type) return Arbitrary_Type;

  function to_str (a : Arbitrary_Type)  return String;

private

	base		: constant integer := 10;

	type Mantissa_Type is array(positive range <>) of integer;
	type Mantissa_Pointer is access Mantissa_Type;

	type Arbitrary_Type(size : positive) is
		new Ada.Finalization.Controlled with record
		mantissa		: Mantissa_Pointer;
		exponent		: integer;
		sign			: integer range -1..1;
		precision	: positive := size;
	end record;

	procedure Initialize(Object : in out Arbitrary_Type);
	procedure Adjust(Object : in out Arbitrary_Type);
	procedure Finalize(Object : in out Arbitrary_Type);


end Arbitrary;
