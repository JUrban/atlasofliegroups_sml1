use "atlas-scripts-sml/polynomial.sml";

(*
  File: atlas-scripts-sml/laurentPolynomial.sml

  Purpose
  - SML translation of `atlas-scripts/laurentPolynomial.at`.
  - Implements Laurent polynomials in an indeterminate `v` with integer
    coefficients, represented as a polynomial multiplied by an integer power:
        v^n * (a0 + a1 v + ... + a_{m-1} v^{m-1})

  Representation
  - A Laurent polynomial is `{w,n}` where:
      - `w : Polynomial.i_poly` is the coefficient list `[a0,...,a_{m-1}]`
      - `n : int` is the exponent shift
    Interpreted as v^n * w(v).

  Notes / limitations
  - This is a direct port of the arithmetic and normalization helpers; the
    example values at the bottom of the `.at` file are omitted.
  - Uses `int` arithmetic and may overflow.
*)

structure LaurentPolynomial = struct
  structure P = Polynomial

  type i_poly = P.i_poly
  type i_laurent_poly = {w: i_poly, n: int}

  fun normalize (f: i_laurent_poly) : i_laurent_poly =
    let
      val w0 = P.strip (#w f)
      val n0 = #n f
    in
      if null w0 then
        {w = P.poly_0, n = 0}
      else
        let
          fun firstNonzero ([], _) = raise Fail "LaurentPolynomial.normalize: impossible"
            | firstNonzero (c :: cs, i) = if c <> 0 then i else firstNonzero (cs, i + 1)
          val r = firstNonzero (w0, 0)
          val w1 = P.strip (List.drop (w0, r))
        in
          {w = w1, n = n0 + r}
        end
    end

  fun degree (f: i_laurent_poly) : int =
    let
      val f = normalize f
    in
      P.degree (#w f) + #n f
    end

  fun lowest_power (f: i_laurent_poly) : int =
    #n (normalize f)

  fun equal (f: i_laurent_poly, g: i_laurent_poly) : bool =
    let
      val f = normalize f
      val g = normalize g
    in
      #n f = #n g andalso #w f = #w g
    end

  fun poly_as_laurent_poly (p: i_poly) : i_laurent_poly =
    {w = P.strip p, n = 0}

  fun laurent_poly_as_poly (f: i_laurent_poly) : i_poly =
    let
      val g = normalize f
      val () = if #n g >= 0 then () else raise Fail "LaurentPolynomial.laurent_poly_as_poly: not a polynomial"
    in
      P.strip (List.tabulate (#n g, fn _ => 0) @ #w g)
    end

  fun constant_laurent_poly (c: int) : i_laurent_poly =
    poly_as_laurent_poly (P.constant_poly c)

  val zero_laurent_poly : i_laurent_poly = constant_laurent_poly 0

  fun v_laurent_power (i: int) : i_laurent_poly = {w = [1], n = i}
  val v_laurent : i_laurent_poly = poly_as_laurent_poly P.poly_q
  fun v_minus_power (k: int) : i_laurent_poly = {w = [1], n = ~k}
  val v_inverse : i_laurent_poly = v_minus_power 1
  val v_laurent_squared : i_laurent_poly = v_laurent_power 2
  val v_minus_two : i_laurent_poly = v_laurent_power (~2)

  (* ignore the integer shift factor *)
  fun poly (f: i_laurent_poly) : i_poly = #w f

  fun shift (f: i_laurent_poly, sh: int) : i_laurent_poly =
    if sh < 0 then raise Fail "LaurentPolynomial.shift: only nonnegative shift allowed"
    else {w = List.tabulate (sh, fn _ => 0) @ #w f, n = #n f - sh}

  fun mulInt (c: int, f: i_laurent_poly) : i_laurent_poly =
    {w = P.strip (List.map (fn x => c * x) (#w f)), n = #n f}

  fun mul (f: i_laurent_poly, g: i_laurent_poly) : i_laurent_poly =
    normalize {w = P.poly_product (#w f, #w g), n = #n f + #n g}

  fun mul_poly_left (p: i_poly, g: i_laurent_poly) : i_laurent_poly =
    mul (poly_as_laurent_poly p, g)

  fun mul_poly_right (f: i_laurent_poly, p: i_poly) : i_laurent_poly =
    mul (f, poly_as_laurent_poly p)

  fun add (f: i_laurent_poly, g: i_laurent_poly) : i_laurent_poly =
    let
      val f = normalize f
      val g = normalize g
    in
      if #n f > #n g then
        let
          val f1 = shift (f, #n f - #n g)
        in
          normalize {w = P.add (#w f1, #w g), n = #n g}
        end
      else
        let
          val g1 = shift (g, #n g - #n f)
        in
          normalize {w = P.add (#w f, #w g1), n = #n f}
        end
    end

  fun sub (f: i_laurent_poly, g: i_laurent_poly) : i_laurent_poly =
    add (f, mulInt (~1, g))

  fun add_int (f: i_laurent_poly, i: int) : i_laurent_poly =
    add (f, constant_laurent_poly i)

  fun sub_int (f: i_laurent_poly, i: int) : i_laurent_poly =
    sub (f, constant_laurent_poly i)

  val v_sum : i_laurent_poly = add (v_laurent, v_inverse)

  (* Substitution v -> 1/v, as in `.at`: reverse coefficients and adjust shift. *)
  fun at_q_inverse (f: i_laurent_poly) : i_laurent_poly =
    let
      val w = P.strip (#w f)
      val n = #n f
      val w' = List.rev w
      val n' = ~n - length w + 1
    in
      normalize {w = w', n = n'}
    end

  fun format (f: i_laurent_poly, q: string) : string =
    let
      val f = normalize f
      val w = #w f
      val n = #n f
      fun termPow k =
        if k > 1 then q ^ "^" ^ Int.toString k
        else if k = 1 then q
        else if k = 0 then ""
        else q ^ "^{" ^ Int.toString k ^ "}"
      fun coefStr (c, k) =
        if c = 0 then ""
        else
          let
            val sign = if c < 0 then "-" else "+"
            val absC = Int.abs c
            val coef =
              if absC = 1 andalso k <> 0 then sign
              else sign ^ Int.toString absC
          in
            coef ^ termPow k
          end
      val parts =
        List.tabulate
          ( length w
          , fn j =>
              let
                val c = List.nth (w, j)
                val k = j + n
              in
                coefStr (c, k)
              end
          )
      val s = String.concat parts
    in
      if null w then "0"
      else if String.size s > 0 andalso String.sub (s, 0) = #"+" then String.extract (s, 1, NONE) else s
    end
end
