--------------------------------------------------------------------------
-- Arbitrary Precision Math Library: Constants
-- Joe Wingbermuehle 20020320 <> 20020327
--------------------------------------------------------------------------

with Arbitrary.Trig; use Arbitrary.Trig;

package body Arbitrary.Const is

	-----------------------------------------------------------------------
	-- Compute precision digits of pi
	-----------------------------------------------------------------------
	function Pi(precision : integer) return Arbitrary_Type is
		result		: Arbitrary_Type(precision);
		one			: constant Arbitrary_Type(precision) :=
							To_Arbitrary(1, precision);
	begin
		-- pi = 4*(44*atan(1/57) + 7*atan(1/239) - 12*atan(1/682)
		--			+ 24*atan(1/12943)

		result := To_Arbitrary(44 * 4, precision)
			* ArcTan(one / To_Arbitrary(57, precision));
		result := result + To_Arbitrary(7 * 4, precision)
			* ArcTan(one / To_Arbitrary(239, precision));
		result := result - To_Arbitrary(12 * 4, precision)
			* ArcTan(one / To_Arbitrary(682, precision));
		result := result + To_Arbitrary(24 * 4, precision)
			* ArcTan(one / To_Arbitrary(12943, precision));

		return result;
	end Pi;

	-----------------------------------------------------------------------
	-- Compute precision digits of the golen ratio
	-----------------------------------------------------------------------
	function Golden_Ratio(precision : integer) return Arbitrary_Type is
		result		: Arbitrary_Type := To_Arbitrary(1, precision);
	begin
		-- golden_ratio = (1 + sqrt(5)) / 2
		result := result + Square_Root(To_Arbitrary(5, precision));
		result := result / To_Arbitrary(2, precision);
		return result;
	end Golden_Ratio;

end Arbitrary.Const;
