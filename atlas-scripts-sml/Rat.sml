structure Rat = struct
  type rat = {num: int, den: int}

  fun gcdIntInf (a: IntInf.int, b: IntInf.int) : IntInf.int =
    let
      val a = IntInf.abs a
      val b = IntInf.abs b
      fun loop (x, 0) = x
        | loop (x, y) = loop (y, IntInf.mod (x, y))
    in
      if a = 0 then b else loop (a, b)
    end

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

  fun make (num: int, den: int) : rat = normalize {num = num, den = den}

  fun add (a: rat, b: rat) : rat =
    normalize {num = #num a * #den b + #num b * #den a, den = #den a * #den b}

  fun sub (a: rat, b: rat) : rat =
    normalize {num = #num a * #den b - #num b * #den a, den = #den a * #den b}

  fun mulInt (a: rat, k: int) : rat = normalize {num = #num a * k, den = #den a}

  fun isNonNeg (a: rat) : bool = #num (normalize a) >= 0

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

