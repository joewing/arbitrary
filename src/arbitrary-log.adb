--------------------------------------------------------------------------
-- Arbitrary Precision Math Library: Logarithmic Functions
-- Joe Wingbermuehle 20020320 <> 20020327
--------------------------------------------------------------------------

with Arbitrary.Trig; use Arbitrary.Trig;

package body Arbitrary.Log is

	-----------------------------------------------------------------------
	-- Compute e^n
	-----------------------------------------------------------------------
	function Exp(a : Arbitrary_Type) return Arbitrary_Type is
		result		: Arbitrary_Type(a.precision);
		multiplier	: Arbitrary_Type(a.precision);
		last			: Arbitrary_Type(a.precision);
		count			: integer;
	begin
		result := To_Arbitrary(2, a.precision);
		count := 2;
		multiplier := a;
		loop
			last := result;
			result := result + multiplier
				* One_Over_Factorial(count, a.precision);
			exit when last = result;
			count := count + 1;
			multiplier := multiplier * multiplier;
		end loop;
		return result;
	end Exp;

	-----------------------------------------------------------------------
	-- Compute ln(2)
	-----------------------------------------------------------------------
	function Ln2(precision : integer) return Arbitrary_Type is
		result		: Arbitrary_Type(precision);
		last			: Arbitrary_Type(precision);
		multiplier	: Arbitrary_Type(precision);
		count			: Arbitrary_Type(precision);
		over_two		: constant Arbitrary_Type(precision) :=
						To_Arbitrary(1, precision) / To_Arbitrary(2, precision);
		one			: constant Arbitrary_Type(precision) :=
						To_Arbitrary(1, precision);
	begin

		-- ln(2) = sum(1/(x*2^x))

		multiplier := over_two;	-- 1/2^1 (1/2^x)
		count := one;
		loop
			last := result;
			result := result + multiplier / count;
			exit when last = result;
			multiplier := multiplier * over_two;
			count := count + one;
		end loop;

		return result;
	end Ln2;

	-----------------------------------------------------------------------
	-- Compute ln(a) for values of a between 0 and 2 exclusive
	-- (convergence is too slow for larger values)
	-----------------------------------------------------------------------
	function Ln_Small(a : Arbitrary_Type) return Arbitrary_Type is
		result		: Arbitrary_Type(a.precision);
		temp			: Arbitrary_Type(a.precision);
		last			: Arbitrary_Type(a.precision);
		count			: Arbitrary_Type(a.precision);
		term			: Arbitrary_Type(a.precision);
		one			: constant Arbitrary_Type(a.precision) :=
							To_Arbitrary(1, a.precision);
		sign			: integer range -1 .. 1;
	begin
		-- ln(1 + x) = x - (1/2)x^2 + (1/3)x^3 - ...
		temp := a - one;
		term := temp;
		count := one;
		sign := 1;
		loop
			last := result;
			if sign > 0 then
				result := result + term / count;
			else
				result := result - term / count;
			end if;
			exit when last = result;
			count := count + one;
			term := term * temp;
			sign := -sign;
		end loop;
		return result;
	end Ln_Small;

	-----------------------------------------------------------------------
	-- Compute ln(x)
	-----------------------------------------------------------------------
	function Ln(a : Arbitrary_Type) return Arbitrary_Type is
		one			: constant Arbitrary_Type(a.precision) :=
							To_Arbitrary(1, a.precision);
		two			: constant Arbitrary_Type(a.precision) :=
							To_Arbitrary(2, a.precision);
	begin
		-- ln(x) = 2*acoth(x - 1) + ln(x - 2)
		if a > two then
			return two * ArcCoth(a - one) + Ln(a - two);
		elsif a = two then
			return Ln2(a.precision);
		else
			return Ln_Small(a);
		end if;
	end Ln;

end Arbitrary.Log;

