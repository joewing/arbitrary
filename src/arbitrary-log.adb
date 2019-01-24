--------------------------------------------------------------------------
-- Arbitrary Precision Math Library: Logarithmic Functions
-- Joe Wingbermuehle 20020320 <> 20020327
--------------------------------------------------------------------------

pragma Ada_2012;
pragma Detect_Blocking;

with Arbitrary.Trig; use Arbitrary.Trig;

package body Arbitrary.Log is

  -----------------------------------------------------------------------
  -- Compute e^n
  -----------------------------------------------------------------------
  function Exp (a : Arbitrary_Type) return Arbitrary_Type is
    result      : Arbitrary_Type    := To_Arbitrary (2, a.precision);
    multiplier  : Arbitrary_Type    := a;
    nlast       : Arbitrary_Type (a.precision);
    count       : Integer           := 2;
  begin
    loop
      nlast := result;
      result := result + multiplier
        * One_Over_Factorial (count, a.precision);
      exit when nlast = result;
      count := count + 1;
      multiplier := multiplier * multiplier;

      nlast := result;
      result := result + multiplier
        * One_Over_Factorial (count, a.precision);
      exit when nlast = result;
      count := count + 1;
      multiplier := multiplier * multiplier;
    end loop;
    return result;
  end Exp;

  -----------------------------------------------------------------------
  -- Compute ln(2)
  -----------------------------------------------------------------------
  function Ln2 (precision : Integer) return Arbitrary_Type is
    result      : Arbitrary_Type (precision);
    nlast       : Arbitrary_Type (precision);
    over_two    : constant Arbitrary_Type -- 1/2^1 (1/2^x)
      :=  To_Arbitrary (1, precision) / To_Arbitrary (2, precision);
    one          : constant Arbitrary_Type (precision)
      :=  To_Arbitrary (1, precision);
    multiplier  : Arbitrary_Type := over_two;
    count        : Arbitrary_Type := one;
  begin
    -- ln(2) = sum(1/(x*2^x))
    loop
      nlast := result;
      result := result + (multiplier / count);
      exit when nlast = result;
      multiplier := multiplier * over_two;
      count := count + one;

      nlast := result;
      result := result + (multiplier / count);
      exit when nlast = result;
      multiplier := multiplier * over_two;
      count := count + one;
    end loop;

    return result;
  end Ln2;

  -----------------------------------------------------------------------
  -- Compute ln(a) for values of a between 0 and 2 exclusive
  -- (convergence is too slow for larger values)
  -----------------------------------------------------------------------
  function Ln_Small (a : Arbitrary_Type) return Arbitrary_Type;

  function Ln_Small (a : Arbitrary_Type) return Arbitrary_Type is
    one     : constant  Arbitrary_Type  :=  To_Arbitrary (1, a.precision);
    temp    : constant  Arbitrary_Type            :=  a - one;
    term    : Arbitrary_Type            :=  temp;
    count   : Arbitrary_Type            :=  one;
    result  : Arbitrary_Type (a.precision);
    nlast   : Arbitrary_Type (a.precision);
    sign    : Integer range -1 .. 1     :=  1;
  begin
    -- ln(1 + x) = x - (1/2)x^2 + (1/3)x^3 - ...
    loop
      nlast   := result; -- "/" highest prececende than "+" and "-"
      result  := result + (if sign > 0 then term / count else -(term / count));
      exit when nlast = result;
      count   := count + one;
      term    := term * temp;
      sign    := -sign;

      nlast   := result; -- "/" highest prececende than "+" and "-"
      result  := result + (if sign > 0 then term / count else -(term / count));
      exit when nlast = result;
      count   := count + one;
      term    := term * temp;
      sign    := -sign;
    end loop;
    return result;
  end Ln_Small;

  -----------------------------------------------------------------------
  -- Compute ln(x)
  -----------------------------------------------------------------------
  function Ln (a : Arbitrary_Type) return Arbitrary_Type is
    one   : constant  Arbitrary_Type  :=  To_Arbitrary (1, a.precision);
    two   : constant  Arbitrary_Type  :=  To_Arbitrary (2, a.precision);
  begin
    -- ln(x) = 2*acoth(x - 1) + ln(x - 2)
    if a > two then
      return two * ArcCoth (a - one) + Ln (a - two);
    elsif a = two then
      return Ln2 (a.precision);
    end if;
    return Ln_Small (a);
  end Ln;

end Arbitrary.Log;
