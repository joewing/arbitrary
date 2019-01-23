--------------------------------------------------------------------------
-- Arbitrary Precision Math Library
-- Joe Wingbermuehle 20020320 <> 20020327
--------------------------------------------------------------------------


pragma Ada_2012;
pragma Detect_Blocking;

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
    object.mantissa.all := (others => 0);
  end Initialize;

  -----------------------------------------------------------------------
  -- Fix an Arbitrary_Type after being assigned a value
  -----------------------------------------------------------------------
  procedure Adjust(object : in out Arbitrary_Type) is
    -- temp    : Mantissa_Pointer;
  begin
    -- temp := new Mantissa_Type'(object.mantissa.all);
    -- object.mantissa := temp;
    object.mantissa := new Mantissa_Type'(object.mantissa.all);
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
  procedure Shift_Left (a : in out Arbitrary_Type; by : Positive := 1);

  procedure Shift_Left (a : in out Arbitrary_Type; by : Positive := 1) is
    bigger  : constant Boolean := by > a.mantissa'Length;
  begin
    if bigger then
      a.mantissa.all := (others => 0);
    else
      a.mantissa(a.mantissa'First .. a.mantissa'Last - by)
        := a.mantissa (a.mantissa'First + by .. a.mantissa'Last);

      a.mantissa(a.mantissa'Last - by + 1 .. a.mantissa'Last)
        := (others => 0);
    end if;
    a.exponent := a.exponent - by;
  end Shift_Left;

  -----------------------------------------------------------------------
  -- Shift the mantissa right one digit and preserve the value
  -----------------------------------------------------------------------
  procedure Shift_Right (a : in out Arbitrary_Type; by : Positive := 1);

  procedure Shift_Right (a : in out Arbitrary_Type; by : Positive := 1) is
    bigger  : constant Boolean := by > a.mantissa'Length;
  begin
    if bigger then
      a.mantissa.all := (others => 0);
    else
      a.mantissa (a.mantissa'First + by .. a.mantissa'Last)
        :=  a.mantissa (a.mantissa'First .. a.mantissa'Last - by);

      a.mantissa (a.mantissa'First .. (a.mantissa'First + by) - 1)
        := (others => 0);
    end if;

    a.exponent := a.exponent + by;
  end Shift_Right;

  -----------------------------------------------------------------------
  -- Fix overflows, underflows, and sign changes
  -----------------------------------------------------------------------
  procedure Normalize(a : in out Arbitrary_Type) is
    changed    : boolean := true;
    temp      : integer;
    carry      : integer;
    left_carry : Natural := 0;
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
        if a.mantissa(x) < 0 then
          changed := true;
          loop
            exit when a.mantissa(x) >= 0;
            a.mantissa(x) := a.mantissa(x) + base;
            a.mantissa(x - 1) := a.mantissa(x - 1) - 1;
          end loop;
        end if;
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
      if a.mantissa(a.mantissa'first) = 0 then
        changed := true;
        for x in a.mantissa'range loop
          exit when a.mantissa(x) /= 0;
          left_carry  := left_carry + 1;
        end loop;
        Shift_Left(a, left_carry);
        left_carry := 0;
      end if;
    end loop;
  end Normalize;

  -----------------------------------------------------------------------
  -- Set an Arbitrary_Type to zero
  -- (This is done in the Initializer)
  -----------------------------------------------------------------------
  procedure Clear(a : out Arbitrary_Type) is
  begin
    a.mantissa.all := (others => 0);
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
    if          a.precision = b.precision
      and then  a.exponent = b.exponent
      and then  a.sign = b.sign
      and then  a.mantissa.all = b.mantissa.all
    then
      return true;
    end if;

    return false;
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
      end if;
      return true;
    elsif a.exponent < b.exponent then
      if a.sign < 0 then
        return true;
      end if;
      return false;
    else
      if a.sign < 0 then
        for x in a.mantissa'range loop
          if a.mantissa(x) = 0 then
            goto continue_loop1;
          end if;
          return (if a.mantissa(x) < b.mantissa(x) then true else false);

          <<continue_loop1>>
        end loop;
      else
        for x in a.mantissa'range loop
          if a.mantissa(x) = 0 then
            goto continue_loop2;
          end if;
          return (if a.mantissa(x) > b.mantissa(x) then true else false);

          <<continue_loop2>>
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
    return (a = b) or else (a > b);
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
      end if;
      return true;
    elsif a.exponent > b.exponent then
      if a.sign < 0 then
        return true;
      end if;
      return false;
    else
      if a.sign < 0 then
        for x in a.mantissa'range loop
          if a.mantissa(x) = 0 then
            goto continue_loop1;
          end if;
          return (if a.mantissa(x) > b.mantissa(x) then true else false);

          <<continue_loop1>>
        end loop;
      else
        for x in a.mantissa'range loop
          if a.mantissa(x) = 0 then
            goto continue_loop2;
          end if;
          return (if a.mantissa(x) < b.mantissa(x) then true else false);

          <<continue_loop2>>
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
    return (a = b) or else (a < b);
  end "<=";

  -----------------------------------------------------------------------
  -- Compute n! to precision digits
  -----------------------------------------------------------------------
  function Factorial(n : integer; precision : integer)
    return Arbitrary_Type is
    result    : Arbitrary_Type(precision) := To_Arbitrary(1, precision);
  begin
    if n < 0 then
      raise Constraint_Error;
    end if;

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
    result    : Arbitrary_Type(precision) := To_Arbitrary(1, precision);
  begin
    if n < 0 then
      raise Constraint_Error;
    end if;
    for x in 2 .. n loop
      result := result / To_Arbitrary(x, precision);
    end loop;
    return result;
  end One_Over_Factorial;

  -----------------------------------------------------------------------
  -- Compute the square root of n to precision digits
  -----------------------------------------------------------------------
  function Square_Root(a : Arbitrary_Type) return Arbitrary_Type is
    result  : Arbitrary_Type(a.precision) := To_Arbitrary(1, a.precision);
    last1   : Arbitrary_Type(a.precision);
    two     : constant Arbitrary_Type(a.precision) :=
              To_Arbitrary(2, a.precision);
  begin
    -- x(i) = (x(i-1) + n / x(i-1)) / 2
    loop
      last1 := result;
      result := result + a / result;
      result := result / two;
      exit when last1 = result;
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
    result    : Arbitrary_Type(a.precision) := a;
  begin
    result.sign := -result.sign;
    return result;
  end "-";

  -----------------------------------------------------------------------
  -- Compute a + b
  -----------------------------------------------------------------------
  function "+"(a, b : Arbitrary_Type) return Arbitrary_Type is
    result      : Arbitrary_Type(a.precision);
    n_index     : integer := 0;
    m_modulo    : integer := 0;
    remainder   : integer := 0;
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
      Shift_Right(result, abs (b.exponent - a.exponent));

      m_modulo  :=  result.mantissa'Length / 4;
      remainder :=  result.mantissa'Length mod 4;
      n_index   := result.mantissa'First;

      for x in 1 .. m_modulo loop  -- unroll loop
        result.mantissa(n_index) := result.mantissa(n_index) + a.mantissa(n_index);
        result.mantissa(n_index + 1) := result.mantissa(n_index + 1) + a.mantissa(n_index + 1);
        result.mantissa(n_index + 2) := result.mantissa(n_index + 2) + a.mantissa(n_index + 2);
        result.mantissa(n_index + 3) := result.mantissa(n_index + 3) + a.mantissa(n_index + 3);
        n_index := n_index + 4;
      end loop;
      if remainder /= 0 then
        result.mantissa(n_index) := result.mantissa(n_index) + a.mantissa(n_index);
        if remainder = 1 then
          goto continue_line1;
        end if;
        result.mantissa(n_index + 1) := result.mantissa(n_index + 1) + a.mantissa(n_index + 1);
        if remainder = 2 then
          goto continue_line1;
        end if;
        result.mantissa(n_index + 2) := result.mantissa(n_index + 2) + a.mantissa(n_index + 2);
        <<continue_line1>>
      end if;

    else
      result := a;

      if a.exponent /= b.exponent then
        Shift_Right(result, abs (b.exponent - a.exponent)); -- <-- need more tests :-)
      end if;

      m_modulo  :=  result.mantissa'Length / 4;
      remainder :=  result.mantissa'Length mod 4;
      n_index   := result.mantissa'First;

      for x in 1 .. m_modulo loop -- unroll loop
        result.mantissa(n_index) := result.mantissa(n_index) + b.mantissa(n_index);
        result.mantissa(n_index + 1) := result.mantissa(n_index + 1) + b.mantissa(n_index + 1);
        result.mantissa(n_index + 2) := result.mantissa(n_index + 2) + b.mantissa(n_index + 2);
        result.mantissa(n_index + 3) := result.mantissa(n_index + 3) + b.mantissa(n_index + 3);
        n_index := n_index + 4;
      end loop;
      if remainder /= 0 then
        result.mantissa(n_index) := result.mantissa(n_index) + b.mantissa(n_index);
        if remainder = 1 then
          goto continue_line2;
        end if;
        result.mantissa(n_index + 1) := result.mantissa(n_index + 1) + b.mantissa(n_index + 1);
        if remainder = 2 then
          goto continue_line2;
        end if;
        result.mantissa(n_index + 2) := result.mantissa(n_index + 2) + b.mantissa(n_index + 2);
        <<continue_line2>>
      end if;
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
      Shift_Right(result, abs (b.exponent - a.exponent));
      for x in result.mantissa'range loop
        result.mantissa(x) := a.mantissa(x) - result.mantissa(x);
      end loop;
    else
      result := a;
      if a.exponent /= b.exponent then
        Shift_Right(result, abs (b.exponent - a.exponent)); -- <-- need more tests :-)
      end if;
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
