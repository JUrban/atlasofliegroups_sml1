(*
  File: atlas-scripts-sml/Rat.sml

  Purpose
  - Tiny rational-number utility used by some of the FPP geometry helpers.
  - Uses `int` storage but computes gcd using `IntInf` to avoid intermediate
    overflow in gcd computations.

  Conventions
  - Rationals are always kept normalized by `normalize`:
      - denominator positive
      - numerator/denominator reduced by gcd
*)
structure Rat = struct
  type rat = {num: int, den: int}

  (* GCD on `IntInf.int` (nonnegative result). *)
  fun gcdIntInf (a: IntInf.int, b: IntInf.int) : IntInf.int =
    let
      val a = IntInf.abs a
      val b = IntInf.abs b
      fun loop (x, 0) = x
        | loop (x, y) = loop (y, IntInf.mod (x, y))
    in
      if a = 0 then b else loop (a, b)
    end

  (* Normalize a rational: reduce by gcd and force `den > 0`. *)
  fun normalize (r: rat) : rat =
    let
      val den0 = #den r
      val num0 = #num r
      val () = if den0 = 0 then raise Fail "Rat.normalize: zero denom" else ()
      val sign = if den0 < 0 then ~1 else 1
      val den1 = den0 * sign
      val num1 = num0 * sign
      val g =
        IntInf.toInt
          (gcdIntInf (IntInf.fromInt den1, IntInf.fromInt num1))
          handle _ => 1
    in
      if g <= 1 then {num = num1, den = den1} else {num = num1 div g, den = den1 div g}
    end

  (* Constructor that normalizes. *)
  fun make (num: int, den: int) : rat = normalize {num = num, den = den}

  (* Rational addition. *)
  fun add (a: rat, b: rat) : rat =
    normalize {num = #num a * #den b + #num b * #den a, den = #den a * #den b}

  (* Rational subtraction. *)
  fun sub (a: rat, b: rat) : rat =
    normalize {num = #num a * #den b - #num b * #den a, den = #den a * #den b}

  (* Multiply by an integer. *)
  fun mulInt (a: rat, k: int) : rat = normalize {num = #num a * k, den = #den a}

  (* Nonnegativity predicate. *)
  fun isNonNeg (a: rat) : bool = #num (normalize a) >= 0

  (* Floor division `⌊a/b⌋` under the assumptions used in this codebase:
       - `a >= 0`
       - `b > 0` *)
  fun divFloor (a: rat, b: rat) : int =
    let
      val a = normalize a
      val b = normalize b
      val () = if #den b = 0 then raise Fail "Rat.divFloor: zero denom" else ()
      val () = if #num b <= 0 then raise Fail "Rat.divFloor: non-positive divisor" else ()
      val () = if #num a < 0 then raise Fail "Rat.divFloor: negative dividend" else ()
      val n = IntInf.fromInt (#num a) * IntInf.fromInt (#den b)
      val d = IntInf.fromInt (#den a) * IntInf.fromInt (#num b)
    in
      IntInf.toInt (IntInf.div (n, d))
    end
end
