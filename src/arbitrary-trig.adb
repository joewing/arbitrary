--------------------------------------------------------------------------
-- Arbitrary Precision Math Library: Trigonometric Functions
-- Joe Wingbermuehle 20020320 <> 20020327
--------------------------------------------------------------------------

with Arbitrary.Log; use Arbitrary.Log;
with Arbitrary.Const; use Arbitrary.Const;

package body Arbitrary.Trig is

	-----------------------------------------------------------------------
	-- Compute the sine of a
	-----------------------------------------------------------------------
	function Sin(a : Arbitrary_Type) return Arbitrary_Type is
		result		: Arbitrary_Type(a.precision);
		last			: Arbitrary_Type(a.precision);
		count			: Arbitrary_Type(a.precision);
		term			: Arbitrary_Type(a.precision);
		one			: constant Arbitrary_Type(a.precision) :=
							To_Arbitrary(1, a.precision);
		sign			: integer range -1 .. 1;
	begin
		-- sin(x) = x - x^3/3! + x^5/5! - x^7/7! + ...
		count := one;
		term := a;
		sign := 1;
		loop
			last := result;
			if sign > 0 then
				result := result + term;
			else
				result := result - term;
			end if;
			exit when last = result;
			count := count + one;
			term := term / count;
			count := count + one;
			term := term / count;
			term := term * a * a;
			sign := -sign;
		end loop;
		return result;
	end Sin;

	-----------------------------------------------------------------------
	-- Compute the cosine of a
	-----------------------------------------------------------------------
	function Cos(a : Arbitrary_Type) return Arbitrary_Type is
		result		: Arbitrary_Type(a.precision);
		last			: Arbitrary_Type(a.precision);
		term			: Arbitrary_Type(a.precision);
		count			: Arbitrary_Type(a.precision);
		one			: Arbitrary_Type(a.precision) :=
							To_Arbitrary(1, a.precision);
		sign			: integer range -1 .. 1;
	begin
		-- cos(x) = 1 - x^2/2! + x^4/4! - x^6/6! + ...
		result := one;
		count := To_Arbitrary(2, a.precision);
		term := a * a / count;
		sign := -1;	
		loop
			last := result;
			if sign > 0 then
				result := result + term;
			else
				result := result - term;
			end if;
			exit when last = result;
			count := count + one;
			term := term / count;
			count := count + one;
			term := term / count;
			term := term * a * a;
			sign := -sign;
		end loop;
		return result;
	end Cos;

	-----------------------------------------------------------------------
	-- Compute the tangent of a
	-----------------------------------------------------------------------
	function Tan(a : Arbitrary_Type) return Arbitrary_Type is
	begin
		-- tan(x) = x + x^3/3 + 2x^5/15 + 17x^7/315 + 62x^9/2835 + ...
		-- tan(x) = sin(x) / cos(x)
		return Sin(a) / Cos(a);
	end Tan;

	-----------------------------------------------------------------------
	-- Compute the cosecant of a
	-----------------------------------------------------------------------
	function Csc(a : Arbitrary_Type) return Arbitrary_Type is
	begin
		-- csc(x) = 1/sin(x)
		return To_Arbitrary(1, a.precision) / Sin(a);
	end Csc;

	-----------------------------------------------------------------------
	-- Compute the secant of a
	-----------------------------------------------------------------------
	function Sec(a : Arbitrary_Type) return Arbitrary_Type is
	begin
		-- sec(x) = 1/cos(x)
		return To_Arbitrary(1, a.precision) / Cos(a);
	end Sec;

	-----------------------------------------------------------------------
	-- Compute the cotangent of a
	-----------------------------------------------------------------------
	function Cot(a : Arbitrary_Type) return Arbitrary_Type is
	begin
		-- cot(x) = cos(x)/sin(x)
		return Cos(a) / Sin(a);
	end Cot;

	-----------------------------------------------------------------------
	-- Compute the inverse sine of a
	-----------------------------------------------------------------------
	function ArcSin(a : Arbitrary_Type) return Arbitrary_Type is
		result		: Arbitrary_Type(a.precision);
		last			: Arbitrary_Type(a.precision);
		term			: Arbitrary_Type(a.precision);
		count			: Arbitrary_Type(a.precision);
		one			: constant Arbitrary_Type(a.precision) :=
							To_Arbitrary(1, a.precision);
	begin
		-- asin(x) = x + (1/6)x^3 + (3/40)x^5 + (5/112)x^7 + ...
		-- each term: 1^2*3^2*5^2*...*(n-2)^2/n! * x^n for n=1,3,5,7,...
		term := a;
		count := one;
		loop
			last := result;
			result := result + term;
			exit when last = result;
			term := term * count * count;
			count := count + one;
			term := term / count;
			count := count + one;
			term := term / count;
			term := term * a * a;
		end loop;
		return result;
	end ArcSin;

	-----------------------------------------------------------------------
	-- Compute the inverse cosine of a
	-----------------------------------------------------------------------
	function ArcCos(a : Arbitrary_Type) return Arbitrary_Type is
		result		: Arbitrary_Type(a.precision);
	begin
		-- acos(x) = pi/2 - x - (1/6)x^3 - (3/40)x^5 - (5/112)x^7 - ...
		-- acos(x) = pi/2 - asin(x)
		result := Pi(a.precision) / To_Arbitrary(2, a.precision);
		result := result - ArcSin(a);
		return result;
	end ArcCos;

	-----------------------------------------------------------------------
	-- Compute the inverse tangent of a
	-----------------------------------------------------------------------
	function ArcTan(a : Arbitrary_Type) return Arbitrary_Type is
		result		: Arbitrary_Type(a.precision);
		last			: Arbitrary_Type(a.precision);
		term			: Arbitrary_Type(a.precision);
		count			: Arbitrary_Type(a.precision);
		two			: constant Arbitrary_Type(a.precision) :=
							To_Arbitrary(2, a.precision);
		sign			: integer range -1 .. 1;
	begin
		-- atan(x) = x - x^3/3 + x^5/5 - ...
		term := a;
		count := To_Arbitrary(1, a.precision);
		sign := 1;
		loop
			last := result;
			if sign > 0 then
				result := result + term / count;
			else
				result := result - term / count;
			end if;
			exit when last = result;
			count := count + two;
			term := term * a * a;
			sign := -sign;
		end loop;
		return result;
	end ArcTan;

	-----------------------------------------------------------------------
	-- Compute the inverse cosecant of a
	-----------------------------------------------------------------------
	function ArcCsc(a : Arbitrary_Type) return Arbitrary_Type is
		result		: Arbitrary_Type(a.precision);
		last			: Arbitrary_Type(a.precision);
		term			: Arbitrary_Type(a.precision);
		count			: Arbitrary_Type(a.precision);
		one			: constant Arbitrary_Type(a.precision) :=
							To_Arbitrary(1, a.precision);
	begin
		-- acsc(x) = x^-1 + (1/6)x^-3 + (3/40)x^-5 + ...
		term := one / a;
		count := one;
		loop
			last := result;
			result := result + term;
			exit when last = result;
			term := term * count * count;
			count := count + one;
			term := term / count;
			count := count + one;
			term := term / count;
			term := term / (a * a);
		end loop;
		return result;
	end ArcCsc;

	-----------------------------------------------------------------------
	-- Compute the inverse secant of a
	-----------------------------------------------------------------------
	function ArcSec(a : Arbitrary_Type) return Arbitrary_Type is
		result		: Arbitrary_Type(a.precision);
	begin
		-- asec(x) = pi/2 - acsc(x)
		result := Pi(a.precision) / To_Arbitrary(2, a.precision);
		result := result - ArcCsc(a);
		return result;
	end ArcSec;

	-----------------------------------------------------------------------
	-- Compute the inverse cotangent of a
	-----------------------------------------------------------------------
	function ArcCot(a : Arbitrary_Type) return Arbitrary_Type is
		result		: Arbitrary_Type(a.precision);
	begin
		-- acot(x) = pi/2 - x + (1/3)x^3 - (1/5)x^5 + ...
		-- acot(x) = pi/2 - atan(x)
		result := Pi(a.precision) / To_Arbitrary(2, a.precision);
		result := result - ArcTan(a);
		return result;
	end ArcCot;

	-----------------------------------------------------------------------
	-- Compute the hyperbolic sine of a
	-----------------------------------------------------------------------
	function Sinh(a : Arbitrary_Type) return Arbitrary_Type is
	begin
		-- sinh(x) = (e^x - e^-x) / 2
		return (Exp(a) - Exp(-a)) / To_Arbitrary(2, a.precision);
	end Sinh;

	-----------------------------------------------------------------------
	-- Compute the hyperbolic cosine of a
	-----------------------------------------------------------------------
	function Cosh(a : Arbitrary_Type) return Arbitrary_Type is
	begin
		-- cosh(x) = (e^x + e^-x) / 2
		return (Exp(a) + Exp(-a)) / To_Arbitrary(2, a.precision);
	end Cosh;

	-----------------------------------------------------------------------
	-- Compute the hyperbolic tangent of a
	-----------------------------------------------------------------------
	function Tanh(a : Arbitrary_Type) return Arbitrary_Type is
		one		: constant Arbitrary_Type(a.precision) :=
						To_Arbitrary(1, a.precision);
		exp2x		: Arbitrary_Type(a.precision);
	begin
		-- tanh(x) = (e^(2x) - 1) / (e^(2x) + 1)
		exp2x := Exp(a * To_Arbitrary(2, a.precision));
		return (exp2x - one) / (exp2x + one);
	end Tanh;

	-----------------------------------------------------------------------
	-- Compute the hyperbolic cosecant of a
	-----------------------------------------------------------------------
	function Csch(a : Arbitrary_Type) return Arbitrary_Type is
	begin
		-- csch(x) = 2 / (e^x - e^-x)
		return To_Arbitrary(2, a.precision) / (Exp(a) - Exp(-a));
	end Csch;

	-----------------------------------------------------------------------
	-- Compute the hyperbolic secant a
	-----------------------------------------------------------------------
	function Sech(a : Arbitrary_Type) return Arbitrary_Type is
	begin
		-- sech(x) = 2 / (e^x + e^-x)
		return To_Arbitrary(2, a.precision) / (Exp(a) + Exp(-a));
	end Sech;

	-----------------------------------------------------------------------
	-- Compute the hyperbolic cotangent of a
	-----------------------------------------------------------------------
	function Coth(a : Arbitrary_Type) return Arbitrary_Type is
		one		: constant Arbitrary_Type(a.precision) :=
						To_Arbitrary(1, a.precision);
		exp2x		: Arbitrary_Type(a.precision);
	begin
		-- coth(x) = (e^(2x) + 1) / (e^(2x) - 1)
		exp2x := Exp(a * To_Arbitrary(2, a.precision));
		return (exp2x + one) / (exp2x - one);
	end Coth;

	-----------------------------------------------------------------------
	-- Compute the inverse hyperbolic sine of a
	-----------------------------------------------------------------------
	function ArcSinh(a : Arbitrary_Type) return Arbitrary_Type is
		result		: Arbitrary_Type(a.precision);
		last			: Arbitrary_Type(a.precision);
		term			: Arbitrary_Type(a.precision);
		count			: Arbitrary_Type(a.precision);
		one			: constant Arbitrary_Type(a.precision) :=
							To_Arbitrary(1, a.precision);
		sign			: integer range -1 .. 1;
	begin
		-- asinh(x) = ln(x + sqrt(1 + x^2))
		-- asinh(x) = x - (1/6)x^3 + (3/40)x^5 - (5/112)x^7 + ...
		term := a;
		count := one;
		sign := 1;
		loop
			last := result;
			if sign > 0 then
				result := result + term;
			else
				result := result - term;
			end if;
			exit when last = result;
			term := term * count * count;
			count := count + one;
			term := term / count;
			count := count + one;
			term := term / count;
			term := term * (a * a);
			sign := -sign;
		end loop;
		return result;
	end ArcSinh;

	-----------------------------------------------------------------------
	-- Compute the inverse hyperbolic cosine
	-----------------------------------------------------------------------
	function ArcCosh(a : Arbitrary_Type) return Arbitrary_Type is
	begin
		-- acosh(x) = ln(x +/- sqrt(x^2 - 1))
		return Ln(a + Square_Root(a * a - To_Arbitrary(1, a.precision)));
	end ArcCosh;

	-----------------------------------------------------------------------
	-- Compute the inverse hyperbolic tangent of a
	-----------------------------------------------------------------------
	function ArcTanh(a : Arbitrary_Type) return Arbitrary_Type is
		result		: Arbitrary_Type(a.precision);
		last			: Arbitrary_Type(a.precision);
		term			: Arbitrary_Type(a.precision);
		count			: Arbitrary_Type(a.precision);
		one			: constant Arbitrary_Type(a.precision) :=
							To_Arbitrary(1, a.precision);
	begin
		-- atanh(x) = x + x^3/3 + x^5/5 + x^7/7 + x^9/9 + ...
		term := a;
		count := one;
		loop
			last := result;
			result := result + term / count;
			exit when last = result;
			count := count + one;
			term := term * a;
		end loop;
		return result;
	end ArcTanh;

	-----------------------------------------------------------------------
	-- Compute the inverse hyperbolic cosecant of a
	-----------------------------------------------------------------------
	function ArcCsch(a : Arbitrary_Type) return Arbitrary_Type is
		result		: Arbitrary_Type(a.precision);
		last			: Arbitrary_Type(a.precision);
		term			: Arbitrary_Type(a.precision);
		count			: Arbitrary_Type(a.precision);
		one			: constant Arbitrary_Type(a.precision) :=
							To_Arbitrary(1, a.precision);
		sign			: integer range  -1 .. 1;
	begin
		-- acsch(x) = x^-1 - (1/6)x^-3 + (3/40)x^-5 - (5/112)x^-7 + ...
		term := one / a;
		count := one;
		sign := 1;
		loop
			last := result;
			if sign > 0 then
				result := result + term;
			else
				result := result - term;
			end if;
			exit when last = result;
			term := term * count * count;
			count := count + one;
			term := term / count;
			count := count + one;
			term := term / count;
			term := term / (a * a);
			sign := -sign;
		end loop;
		return result;
	end ArcCsch;

	-----------------------------------------------------------------------
	-- Compute the inverse hyperbolic secant of a
	-----------------------------------------------------------------------
	function ArcSech(a : Arbitrary_Type) return Arbitrary_Type is
		one		: constant Arbitrary_Type(a.precision) :=
						To_Arbitrary(1, a.precision);
	begin
		-- asech(x) = ln((1 +/- sqrt(1 - x^2)) / x)
		return Ln((one + Square_Root(one - a * a)) / a);
	end ArcSech;

	-----------------------------------------------------------------------
	-- Compute the inverse hyperbolic cotangent of a
	-----------------------------------------------------------------------
	function ArcCoth(a : Arbitrary_Type) return Arbitrary_Type is
		result		: Arbitrary_Type(a.precision);
		last			: Arbitrary_Type(a.precision);
		term			: Arbitrary_Type(a.precision);
		count			: Arbitrary_Type(a.precision);
		one			: constant Arbitrary_Type(a.precision) :=
							To_Arbitrary(1, a.precision);
		two			: constant Arbitrary_Type(a.precision) :=
							To_Arbitrary(2, a.precision);
	begin
		-- acoth(x) = x^-1 + (1/3)x^-3 + (1/5)x^-5 + ...
		term := one / a;
		count := one;
		loop
			last := result;
			result := result + term / count;
			exit when last = result;
			count := count + two;
			term := term / (a * a);
		end loop;
		return result;
	end ArcCoth;

end Arbitrary.Trig;

