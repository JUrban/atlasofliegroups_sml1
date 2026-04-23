(*  File: atlas-scripts-sml/Split.sml

    Purpose
    - Standard ML representation of Atlas “split integers” `a + s*b` used in the
      `.at` scripts (and in Atlas `Split_integer`), where `s^2 = 1`.
    - This is a small, self-contained port of the Split-integer utilities from
      `atlas-scripts/basic.at` (see the “Split integers” section).

    Representation
    - `t = {e,s}` corresponds to `e + s*s` in the `.at` notation:
        - `e`: integer part `a`
        - `s`: coefficient of the formal symbol `s`
    - Arithmetic is over `Z[s]/(s^2-1)`:
        (a + s*b) * (c + s*d) = (ac + bd) + s(ad + bc).

    Notes
    - We use `IntInf.int` to avoid overflows (some Atlas coefficients can grow).
    - Many `.at` scripts only need `exp_s`, `times_s`, and `split_as_int`.
*)

structure Split = struct
  type t = {e: IntInf.int, s: IntInf.int}

  fun fromInt (n: int) : t = {e = IntInf.fromInt n, s = 0}
  fun fromParts (e: IntInf.int, s: IntInf.int) : t = {e = e, s = s}

  val zero : t = {e = 0, s = 0}
  val one : t = {e = 1, s = 0}
  val minus_one : t = {e = ~1, s = 0}
  val s : t = {e = 0, s = 1}
  val one_minus_s : t = {e = 1, s = ~1}
  val one_plus_s : t = {e = 1, s = 1}

  fun int_part ({e, ...}: t) : IntInf.int = e
  fun s_part ({s, ...}: t) : IntInf.int = s

  fun isZero ({e, s}: t) : bool = (e = 0 andalso s = 0)
  fun is_pure ({e, s}: t) : bool = (e = 0 orelse s = 0)

  fun neg ({e, s}: t) : t = {e = ~e, s = ~s}
  fun add ({e = a, s = b}: t, {e = c, s = d}: t) : t = {e = a + c, s = b + d}
  fun sub (x: t, y: t) : t = add (x, neg y)

  fun times_s ({e, s}: t) : t = {e = s, s = e}

  fun mul ({e = a, s = b}: t, {e = c, s = d}: t) : t =
    {e = a * c + b * d, s = a * d + b * c}

  fun s_to_1 ({e, s}: t) : IntInf.int = e + s
  fun s_to_minus_1 ({e, s}: t) : IntInf.int = e - s

  fun split_as_int ({e, s}: t) : IntInf.int =
    if s = 0 then e else raise Fail "Split.split_as_int: split integer is not an integer (s-part nonzero)"

  (* Component-wise Euclidean division by a (nonzero) integer `n`. *)
  fun divModInt ({e, s}: t, n: int) : t * t =
    if n = 0 then
      raise Fail "Split.divModInt: divide by zero"
    else
      let
        val n' = IntInf.fromInt n
        val (qe, re) = IntInf.divMod (e, n')
        val (qs, rs) = IntInf.divMod (s, n')
      in
        ({e = qe, s = qs}, {e = re, s = rs})
      end

  fun half (w: t) : t =
    let
      val (q, r) = divModInt (w, 2)
    in
      if isZero r then q else raise Fail "Split.half: inexact halving"
    end

  fun divExact (w: t, n: int) : t =
    let
      val (q, r) = divModInt (w, n)
    in
      if isZero r then q else raise Fail "Split.divExact: inexact division"
    end

  fun modInt ({e, s}: t, n: int) : t =
    if n = 0 then raise Fail "Split.modInt: mod by zero"
    else
      let
        val n' = IntInf.fromInt n
      in
        {e = IntInf.mod (e, n'), s = IntInf.mod (s, n')}
      end

  fun exp_s (n: int) : t = if n mod 2 = 0 then one else s

  fun pow (x: t, n: int) : t =
    if n < 0 then
      raise Fail "Split.pow: negative exponent"
    else
      let
        fun loop (acc, base, k) =
          if k = 0 then acc
          else if k mod 2 = 1 then loop (mul (acc, base), mul (base, base), k div 2)
          else loop (acc, mul (base, base), k div 2)
      in
        loop (one, x, n)
      end

  fun intToString (z: IntInf.int) : string = IntInf.toString z

  (* Formatting helpers closely matching `split_format` / `split_factor_format`
     from `basic.at` (purely cosmetic). *)
  fun split_format ({e, s}: t) : string =
    if e = 0 then
      (case (IntInf.compare (s, ~1), IntInf.compare (s, 0), IntInf.compare (s, 1)) of
         (EQUAL, _, _) => "-s"
       | (_, EQUAL, _) => "0"
       | (_, _, EQUAL) => "s"
       | _ => intToString s ^ "s")
    else
      let
        val base = intToString e
      in
        if s = 0 then
          base
        else if s = 1 then
          base ^ "+s"
        else if s = ~1 then
          base ^ "-s"
        else if s > 0 then
          base ^ "+" ^ intToString s ^ "s"
        else
          base ^ intToString s ^ "s"
      end

  fun split_factor_format (w: t) : string =
    if is_pure w then split_format w else "(" ^ split_format w ^ ")"
end

