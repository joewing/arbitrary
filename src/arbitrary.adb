--------------------------------------------------------------------------
-- Arbitrary Precision Math Library
-- Joe Wingbermuehle 20020320 <> 20020327
--------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package body Arbitrary is

  procedure Delete is new Ada.Unchecked_Deallocation(Mantissa_Type,
    Mantissa_Pointer);

  -----------------------------------------------------------------------
  -- Initialize an Arbitrary_Type
  -----------------------------------------------------------------------
  procedure Initialize (object : in out Arbitrary_Type) is
  begin
    object.mantissa := new Mantissa_Type (1..object.precision);
    object.exponent := 0;
    object.sign := 1;
    -- for x in object.mantissa'range loop
    --   object.mantissa(x) := 0;
    -- end loop;
    object.mantissa.all := (others => 0);
  end Initialize;

  -----------------------------------------------------------------------
  -- Fix an Arbitrary_Type after being assigned a value
  -----------------------------------------------------------------------
  procedure Adjust(object : in out Arbitrary_Type) is
    temp    : Mantissa_Pointer;
  begin
    temp := new Mantissa_Type(1..object.precision);
    -- for x in object.mantissa'range loop
    --   temp(x) := object.mantissa(x);
    -- end loop;
    temp.all := object.mantissa.all; -- ? add a object.mantissa.all /= null ?
    object.mantissa := temp;
  end Adjust;

  -----------------------------------------------------------------------
  -- Release an Arbitrary_Type;
  -----------------------------------------------------------------------
  procedure Finalize(object : in out Arbitrary_Type) is
  begin
    if object.mantissa /= null then
      Delete(object.mantissa);
    end if;
    object.mantissa := null;
  end Finalize;

  -----------------------------------------------------------------------
  -- Shift mantissa left one digit and preserve the value
  -----------------------------------------------------------------------
  -- procedure Shift_Left(a : in out Arbitrary_Type) is
  -- begin
  --   for x in a.mantissa'first + 1..a.mantissa'last loop
  --     a.mantissa(x - 1) := a.mantissa(x);
  --   end loop;
  --   a.mantissa(a.mantissa'last) := 0;
  --   a.exponent := a.exponent - 1;
  -- end Shift_Left;
  procedure Shift_Left (a : in out Arbitrary_Type; by : Positive := 1);

  procedure Shift_Left (a : in out Arbitrary_Type; by : Positive := 1) is
    -- by2       : constant Natural := (if by > 0 then by else 1);
    data_tmp  : constant Mantissa_Type :=
      a.mantissa.all (a.mantissa.all'First + by .. a.mantissa.all'Last);
  begin

    a.mantissa.all (a.mantissa.all'First .. a.mantissa.all'Last - by)
      := data_tmp;

    a.mantissa.all (a.mantissa.all'Last - by + 1 .. a.mantissa.all'Last)
      := (others => 0);
    a.exponent := a.exponent - by;
  end Shift_Left;

  -----------------------------------------------------------------------
  -- Shift the mantissa right one digit and preserve the value
  -----------------------------------------------------------------------
  procedure Shift_Right(a : in out Arbitrary_Type) is
  begin
    for x in reverse a.mantissa'first..a.mantissa'last - 1 loop
      a.mantissa(x + 1) := a.mantissa(x);
    end loop;
    a.mantissa(a.mantissa'first) := 0;
    a.exponent := a.exponent + 1;
  end Shift_Right;

  -----------------------------------------------------------------------
  -- Fix overflows, underflows, and sign changes
  -----------------------------------------------------------------------
  procedure Normalize(a : in out Arbitrary_Type) is
    changed    : boolean := true;
    temp      : integer;
    carry      : integer;
  begin
    -- Zero is a special case
    temp := a.mantissa'first;
    while temp <= a.mantissa'last loop
      exit when a.mantissa(temp) /= 0;
      temp := temp + 1;
    end loop;
    if temp > a.mantissa'last then
      a.sign := 1;
      a.exponent := 0;
      return;
    end if;

    while changed loop
      changed := false;
      for x in a.mantissa'first + 1 .. a.mantissa'last loop
        if a.mantissa(x) >= base then
          temp := a.mantissa(x);
          a.mantissa(x) := temp mod base;
          a.mantissa(x - 1) := a.mantissa(x - 1) + temp / base;
          changed := true;
        end if;
        while a.mantissa(x) < 0 loop
          a.mantissa(x) := a.mantissa(x) + base;
          a.mantissa(x - 1) := a.mantissa(x - 1) - 1;
          changed := true;
        end loop;
      end loop;
      if a.mantissa(a.mantissa'first) >= base then
        carry := a.mantissa(a.mantissa'first) / base;
        temp := a.mantissa(a.mantissa'first) mod base;
        a.mantissa(a.mantissa'first) := temp;
        Shift_Right(a);
        a.mantissa(a.mantissa'first) := carry;
        changed := true;
      end if;
      if a.mantissa(a.mantissa'first) < 0 then
        for x in a.mantissa'range loop
          a.mantissa(x) := -a.mantissa(x);
        end loop;
        a.sign := -a.sign;
        changed := true;
      end if;
      while a.mantissa(a.mantissa'first) = 0 loop
        Shift_Left(a);
        changed := true;
      end loop;
    end loop;
  end Normalize;

  -----------------------------------------------------------------------
  -- Display an Arbitrary_Type
  -----------------------------------------------------------------------
  procedure Display(a : Arbitrary_Type) is
  begin
    if a.sign < 0 then
      Put("-");
    end if;
    Put(character'val(a.mantissa(a.mantissa'first) + character'pos('0')));
    Put(".");
    for x in a.mantissa'first + 1 .. a.mantissa'last loop
      Put(character'val(a.mantissa(x) + character'pos('0')));
    end loop;
    if a.exponent /= 0 then
      Put(" E" & a.exponent'img);
    end if;
  end Display;

  -----------------------------------------------------------------------
  -- Set an Arbitrary_Type to zero
  -- (This is done in the Initializer)
  -----------------------------------------------------------------------
  procedure Clear(a : out Arbitrary_Type) is
  begin
    for x in a.mantissa'range loop
      a.mantissa(x) := 0;
    end loop;
    a.exponent := 0;
    a.sign := 1;
  end Clear;

  -----------------------------------------------------------------------
  -- Convert an integer type to an Arbitrary_Type
  -----------------------------------------------------------------------
  function To_Arbitrary(value : integer; precision : integer)
    return Arbitrary_Type is
    result    : Arbitrary_Type(precision);
  begin
    result.mantissa(result.exponent + 1) := value;
    Normalize(result);
    return result;
  end To_Arbitrary;

  -----------------------------------------------------------------------
  -- Test for equality
  -----------------------------------------------------------------------
  function "="(a, b : Arbitrary_Type) return boolean is
  begin
    if a.precision = b.precision
      and a.exponent = b.exponent
      and a.sign = b.sign then
      for x in a.mantissa'first..a.mantissa'last loop
        if a.mantissa(x) /= b.mantissa(x) then
          return false;
        end if;
      end loop;
      return true;
    else
      return false;
    end if;
  end "=";

  -----------------------------------------------------------------------
  -- Test greater than
  -----------------------------------------------------------------------
  function ">"(a, b : Arbitrary_Type) return boolean is
  begin
    if DEBUG_CHECKS then
      if a.precision /= b.precision then
        raise Constraint_Error;
      end if;
    end if;
    if a.sign < 0 and b.sign > 0 then
      return false;
    elsif a.sign > 0 and b.sign < 0 then
      return true;
    elsif a.exponent > b.exponent then
      if a.sign < 0 then
        return false;
      else
        return true;
      end if;
    elsif a.exponent < b.exponent then
      if a.sign < 0 then
        return true;
      else
        return false;
      end if;
    else
      if a.sign < 0 then
        for x in a.mantissa'range loop
          if a.mantissa(x) < b.mantissa(x) then
            return true;
          elsif a.mantissa(x) > b.mantissa(x) then
            return false;
          end if;
        end loop;
      else
        for x in a.mantissa'range loop
          if a.mantissa(x) > b.mantissa(x) then
            return true;
          elsif a.mantissa(x) < b.mantissa(x) then
            return false;
          end if;
        end loop;
      end if;
      return false;
    end if;
  end ">";

  -----------------------------------------------------------------------
  -- Test greater or equal
  -----------------------------------------------------------------------
  function ">="(a, b : Arbitrary_Type) return boolean is
  begin
    if DEBUG_CHECKS then
      if a.precision /= b.precision then
        raise Constraint_Error;
      end if;
    end if;
    if a.sign < 0 and b.sign > 0 then
      return false;
    elsif a.sign > 0 and b.sign < 0 then
      return true;
    elsif a.exponent > b.exponent then
      if a.sign < 0 then
        return false;
      else
        return true;
      end if;
    elsif a.exponent < b.exponent then
      if a.sign < 0 then
        return true;
      else
        return false;
      end if;
    else
      if a.sign < 0 then
        for x in a.mantissa'range loop
          if a.mantissa(x) < b.mantissa(x) then
            return true;
          elsif a.mantissa(x) > b.mantissa(x) then
            return false;
          end if;
        end loop;
      else
        for x in a.mantissa'range loop
          if a.mantissa(x) > b.mantissa(x) then
            return true;
          elsif a.mantissa(x) < b.mantissa(x) then
            return false;
          end if;
        end loop;
      end if;
      return true;
    end if;
  end ">=";

  -----------------------------------------------------------------------
  -- Test if less than
  -----------------------------------------------------------------------
  function "<"(a, b : Arbitrary_Type) return boolean is
  begin
    if DEBUG_CHECKS then
      if a.precision /= b.precision then
        raise Constraint_Error;
      end if;
    end if;
    if a.sign < 0 and b.sign > 0 then
      return true;
    elsif a.sign > 0 and b.sign < 0 then
      return false;
    elsif a.exponent < b.exponent then
      if a.sign < 0 then
        return false;
      else
        return true;
      end if;
    elsif a.exponent > b.exponent then
      if a.sign < 0 then
        return true;
      else
        return false;
      end if;
    else
      if a.sign < 0 then
        for x in a.mantissa'range loop
          if a.mantissa(x) > b.mantissa(x) then
            return true;
          elsif a.mantissa(x) < b.mantissa(x) then
            return false;
          end if;
        end loop;
      else
        for x in a.mantissa'range loop
          if a.mantissa(x) < b.mantissa(x) then
            return true;
          elsif a.mantissa(x) > b.mantissa(x) then
            return false;
          end if;
        end loop;
      end if;
      return false;
    end if;
  end "<";

  -----------------------------------------------------------------------
  -- Test if less than or equal to
  -----------------------------------------------------------------------
  function "<="(a, b : Arbitrary_Type) return boolean is
  begin
    if DEBUG_CHECKS then
      if a.precision /= b.precision then
        raise Constraint_Error;
      end if;
    end if;
    if a.sign < 0 and b.sign > 0 then
      return true;
    elsif a.sign > 0 and b.sign < 0 then
      return false;
    elsif a.exponent < b.exponent then
      if a.sign < 0 then
        return false;
      else
        return true;
      end if;
    elsif a.exponent > b.exponent then
      if a.sign < 0 then
        return true;
      else
        return false;
      end if;
    else
      if a.sign < 0 then
        for x in a.mantissa'range loop
          if a.mantissa(x) > b.mantissa(x) then
            return true;
          elsif a.mantissa(x) < b.mantissa(x) then
            return false;
          end if;
        end loop;
      else
        for x in a.mantissa'range loop
          if a.mantissa(x) < b.mantissa(x) then
            return true;
          elsif a.mantissa(x) > b.mantissa(x) then
            return false;
          end if;
        end loop;
      end if;
      return true;
    end if;
  end "<=";

  -----------------------------------------------------------------------
  -- Compute n! to precision digits
  -----------------------------------------------------------------------
  function Factorial(n : integer; precision : integer)
    return Arbitrary_Type is
    result    : Arbitrary_Type(precision);
  begin
    if n < 0 then
      raise Constraint_Error;
    end if;
    result := To_Arbitrary(1, precision);
    for x in 2 .. n loop
      result := result * To_Arbitrary(x, precision);
    end loop;
    return result;
  end Factorial;

  -----------------------------------------------------------------------
  -- Compute 1/n!
  -----------------------------------------------------------------------
  function One_Over_Factorial(n : integer; precision : integer)
    return Arbitrary_Type is
    result    : Arbitrary_Type(precision);
  begin
    if n < 0 then
      raise Constraint_Error;
    end if;
    result := To_Arbitrary(1, precision);
    for x in 2 .. n loop
      result := result / To_Arbitrary(x, precision);
    end loop;
    return result;
  end One_Over_Factorial;

  -----------------------------------------------------------------------
  -- Compute the square root of n to precision digits
  -----------------------------------------------------------------------
  function Square_Root(a : Arbitrary_Type) return Arbitrary_Type is
    result    : Arbitrary_Type(a.precision);
    last      : Arbitrary_Type(a.precision);
    two      : constant Arbitrary_Type(a.precision) :=
              To_Arbitrary(2, a.precision);
  begin
    -- x(i) = (x(i-1) + n / x(i-1)) / 2
    result := To_Arbitrary(1, a.precision);
    loop
      last := result;
      result := result + a / result;
      result := result / two;
      exit when last = result;
    end loop;
    return result;
  end Square_Root;

  -----------------------------------------------------------------------
  -- Unary + operator -- do nothing
  -----------------------------------------------------------------------
  function "+"(a : Arbitrary_Type) return Arbitrary_Type is
  begin
    return a;
  end "+";

  -----------------------------------------------------------------------
  -- Negate a
  -----------------------------------------------------------------------
  function "-"(a : Arbitrary_Type) return Arbitrary_Type is
    result    : Arbitrary_Type(a.precision);
  begin
    result := a;
    result.sign := -result.sign;
    return result;
  end "-";

  -----------------------------------------------------------------------
  -- Compute a + b
  -----------------------------------------------------------------------
  function "+"(a, b : Arbitrary_Type) return Arbitrary_Type is
    result    : Arbitrary_Type(a.precision);
  begin
    if DEBUG_CHECKS then
      if a.precision /= b.precision then
        raise Constraint_Error;
      end if;
    end if;

    -- If the signs are different, addition becomes subtraction
    if a.sign /= b.sign then
      if b.sign < 0 then
        result := b;
        result.sign := -result.sign;
        result := a - result;
        return result;
      else
        result := a;
        result.sign := -result.sign;
        result := b - result;
        return result;
      end if;
    end if;

    -- Set result to the additive with the least exponent and shift
    if a.exponent > b.exponent then
      result := b;
      for x in b.exponent .. a.exponent - 1 loop
        Shift_Right(result);
      end loop;
      for x in result.mantissa'range loop
        result.mantissa(x) := result.mantissa(x) + a.mantissa(x);
      end loop;
    else
      result := a;
      for x in a.exponent .. b.exponent - 1 loop
        Shift_Right(result);
      end loop;
      for x in result.mantissa'range loop
        result.mantissa(x) := result.mantissa(x) + b.mantissa(x);
      end loop;
    end if;
    Normalize(result);
    return result;
  end "+";

  -----------------------------------------------------------------------
  -- Compute a - b
  -----------------------------------------------------------------------
  function "-"(a, b : Arbitrary_Type) return Arbitrary_Type is
    result    : Arbitrary_Type(a.precision);
  begin
    if DEBUG_CHECKS then
      if a.precision /= b.precision then
        raise Constraint_Error;
      end if;
    end if;
    -- Use addition if the signs differ
    if a.sign /= b.sign then
      if a.sign < 0 then
        result := b;
        result.sign := -1;
        return a + result;
      else
        result := b;
        result.sign := 1;
        return a + result;
      end if;
    end if;

    if a.exponent > b.exponent then
      result := b;
      for x in b.exponent .. a.exponent - 1 loop
        Shift_Right(result);
      end loop;
      for x in result.mantissa'range loop
        result.mantissa(x) := a.mantissa(x) - result.mantissa(x);
      end loop;
    else
      result := a;
      for x in a.exponent .. b.exponent - 1 loop
        Shift_Right(result);
      end loop;
      for x in result.mantissa'range loop
        result.mantissa(x) := result.mantissa(x) - b.mantissa(x);
      end loop;
    end if;
    Normalize(result);
    return result;
  end "-";

  -----------------------------------------------------------------------
  -- Compute a * b
  -----------------------------------------------------------------------
  function "*"(a, b : Arbitrary_Type) return Arbitrary_Type is
    result    : Arbitrary_Type(
      integer'max(a.precision, b.precision));
    offset    : integer;  -- offset in result;
  begin
    if DEBUG_CHECKS then
      if a.precision /= b.precision then
        raise Constraint_Error;
      end if;
    end if;
    offset := 0;
    for x in b.mantissa'range loop
      for y in a.mantissa'first .. a.mantissa'last - offset loop
        result.mantissa(offset + y) :=
          result.mantissa(offset + y) + a.mantissa(y) * b.mantissa(x);
      end loop;
      offset := offset + 1;
    end loop;
    result.sign := a.sign * b.sign;
    result.exponent := a.exponent + b.exponent;
    Normalize(result);
    return result;
  end "*";

  -----------------------------------------------------------------------
  -- Compute a / b
  -----------------------------------------------------------------------
  function "/"(a, b : Arbitrary_Type) return Arbitrary_Type is
    result    : Arbitrary_Type(a.precision);
    denominator  : Arbitrary_Type(a.precision);
    numerator  : Arbitrary_Type(a.precision);
    temp      : integer;
  begin
    if DEBUG_CHECKS then
      if a.precision /= b.precision then
        raise Constraint_Error;
      end if;
      if b = To_Arbitrary(0, b.precision) then
        raise Constraint_Error;
      end if;
    end if;
    if a = To_Arbitrary(0, a.precision) then
      return To_Arbitrary(0, a.precision);
    end if;
    numerator := a;
    numerator.sign := 1;
    denominator := b;
    denominator.sign := 1;

    -- The result's exponent will be the numerator's exponent
    -- minus the denominators exponent
    temp := numerator.exponent - denominator.exponent;
    result.exponent := temp;

    -- Now adjust the denominator's exponent such that we start getting
    -- digits for the result immediately
    -- The first digits will arise when the numerator and denominator
    -- have the same exponent. Since the result's exponent has already
    -- been calcuated, we simply adjust the denominator
    denominator.exponent := numerator.exponent;

    Magnitude: for x in result.mantissa'range loop
      Digit_Count: while numerator >= denominator loop
        -- Note that numerator must always be normalized
        exit Magnitude when
          numerator.mantissa(numerator.mantissa'first) = 0;
        result.mantissa(x) := result.mantissa(x) + 1;
        numerator := numerator - denominator;
      end loop Digit_Count;
      denominator.exponent := denominator.exponent - 1;
    end loop Magnitude;
    result.sign := a.sign * b.sign;
    Normalize(result);
    return result;
  end "/";

  function to_str (a : Arbitrary_Type)  return String
  is
    chr_pos   : constant  Natural   :=  Character'Pos ('0');
    exp_str   : constant  String    :=  a.exponent'Image;
    temp_str  : String (1 .. a.mantissa.all'Length + exp_str'Length + 7) :=
      (others => '0');
    n_index   : Natural             :=  temp_str'First;
    m_first   : constant  Natural   :=  a.mantissa.all'First;
  begin
    if a.sign < 0 then
      temp_str (n_index)  := '-';
      n_index :=  n_index + 1;
    end if;
    temp_str (n_index)  :=  Character'Val (a.mantissa.all (m_first) + chr_pos);
    temp_str (n_index + 1)  := '.';
    n_index :=  n_index + 2;

    loop1 :
    for E of a.mantissa.all (m_first + 1 .. a.mantissa.all'Last) loop
      temp_str (n_index)  :=  Character'Val (E + chr_pos);
      n_index :=  n_index + 1;
    end loop loop1;
    -- n_index :=  n_index + 1;

    if a.exponent /= 0 then
      temp_str (n_index)      :=  'E';
      temp_str (n_index + 1)  :=  (if a.exponent > 0 then '+' else '-');

      n_index :=  n_index + 2;

      temp_str (n_index .. n_index + exp_str'Length - 2)  :=
        exp_str (exp_str'First + 1 .. exp_str'Last);
      n_index                 :=  n_index + exp_str'Length - 2;
    end if;

    return temp_str (1 .. n_index);
  end to_str;


end Arbitrary;
