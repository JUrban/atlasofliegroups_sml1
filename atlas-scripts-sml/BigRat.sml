(* 
  File: atlas-scripts-sml/BigRat.sml

  Purpose
  - Simple exact rationals backed by `IntInf.int`.
  - Intended for ports of `.at` scripts that rely on exact rational arithmetic
    where `int`-based rationals would overflow too easily.

  Conventions
  - Values are normalized:
      - denominator positive
      - gcd(num,den)=1
*)

structure BigRat = struct
  type t = {num: IntInf.int, den: IntInf.int}

  fun gcd (a: IntInf.int, b: IntInf.int) : IntInf.int =
    let
      val a = IntInf.abs a
      val b = IntInf.abs b
      fun loop (x, 0) = x
        | loop (x, y) = loop (y, IntInf.mod (x, y))
    in
      if a = 0 then b else loop (a, b)
    end

  fun normalize (q: t) : t =
    let
      val den0 = #den q
      val num0 = #num q
      val () = if den0 = 0 then raise Fail "BigRat.normalize: zero denom" else ()
      val sign = if den0 < 0 then ~1 else 1
      val den1 = den0 * sign
      val num1 = num0 * sign
      val g = gcd (num1, den1)
    in
      if g <= 1 then {num = num1, den = den1} else {num = IntInf.div (num1, g), den = IntInf.div (den1, g)}
    end

  fun make (num: IntInf.int, den: IntInf.int) : t = normalize {num = num, den = den}
  fun fromInt (n: int) : t = {num = IntInf.fromInt n, den = 1}
  fun zero () : t = {num = 0, den = 1}
  fun one () : t = {num = 1, den = 1}

  fun isZero (q: t) : bool = #num (normalize q) = 0

  fun neg (q: t) : t = normalize {num = ~ (#num q), den = #den q}

  fun add (a: t, b: t) : t =
    normalize {num = #num a * #den b + #num b * #den a, den = #den a * #den b}

  fun sub (a: t, b: t) : t =
    normalize {num = #num a * #den b - #num b * #den a, den = #den a * #den b}

  fun mul (a: t, b: t) : t =
    normalize {num = #num a * #num b, den = #den a * #den b}

  fun mulInt (a: t, k: int) : t =
    normalize {num = #num a * IntInf.fromInt k, den = #den a}

  fun inv (a: t) : t =
    let
      val a = normalize a
      val () = if #num a = 0 then raise Fail "BigRat.inv: divide by zero" else ()
    in
      normalize {num = #den a, den = #num a}
    end

  fun divRat (a: t, b: t) : t = mul (a, inv b)

  fun equal (a: t, b: t) : bool =
    let
      val a = normalize a
      val b = normalize b
    in
      #num a = #num b andalso #den a = #den b
    end
end
