use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/Coordinates.sml

  Purpose
  - Convert Atlas weights into coordinate vectors with respect to simple coroots,
    and perform basic checks used by the FPP routines (e.g. membership in the
    unit cube `[0,1]^r`).

  Key conventions
  - A rational weight is represented as `{den, nums}` meaning `nums/den`.
  - Coordinates are computed by pairing with simple coroot vectors:
      coord_i = <alpha_i^vee, weight>.
*)
structure Coordinates = struct
  type rat = {num: int, den: int}
  type ratweight = {den: int, nums: int list}

  (* Parse whitespace-separated integers. *)
  fun parseInts s =
    let
      fun toInt tok =
        case Int.fromString tok of
          SOME n => n
        | NONE => raise Fail ("Coordinates: bad int token: " ^ tok)
    in
      List.map toInt (String.tokens Char.isSpace s)
    end

  (* Parse a rational weight `den n1 ... nk`. *)
  fun parseRatWeightText s : ratweight =
    (case parseInts s of
       den :: rest => {den = den, nums = rest}
     | _ => raise Fail ("Coordinates: bad ratweight text: " ^ s))

  (* Parse the `atlas_group_simple_coroots_text` format:
       `ssRank rank` then `ssRank` coroot vectors of length `rank`. *)
  fun parseSimpleCorootsText s : int list list =
    let
      val ns = parseInts s
    in
      case ns of
        ssRank :: rank :: rest =>
          let
            fun takeVec (0, xs, acc) = (List.rev acc, xs)
              | takeVec (n, x :: xs, acc) = takeVec (n - 1, xs, x :: acc)
              | takeVec _ = raise Fail "Coordinates: truncated coroots"
            fun loop (0, xs, acc) = (List.rev acc, xs)
              | loop (k, xs, acc) =
                  let
                    val (v, xs') = takeVec (rank, xs, [])
                  in
                    loop (k - 1, xs', v :: acc)
                  end
            val (cors, leftover) = loop (ssRank, rest, [])
          in
            if null leftover then cors else raise Fail "Coordinates: extra coroot ints"
          end
      | _ => raise Fail "Coordinates: bad coroot header"
    end

  (* Dot product of integer vectors. *)
  fun dot (xs: int list, ys: int list) : int =
    let
      fun loop ([], [], acc) = acc
        | loop (a :: as', b :: bs', acc) = loop (as', bs', acc + a * b)
        | loop _ = raise Fail "Coordinates.dot: length mismatch"
    in
      loop (xs, ys, 0)
    end

  (* Integer gcd (nonnegative result). *)
  fun gcd (a: int, b: int) : int =
    let
      val a = Int.abs a
      val b = Int.abs b
      fun loop (x, 0) = x
        | loop (x, y) = loop (y, x mod y)
    in
      if a = 0 then b else loop (a, b)
    end

  (* Normalize a rational (reduce and force positive denominator). *)
  fun normRat ({num, den}: rat) : rat =
    if den = 0 then raise Fail "Coordinates.normRat: zero denom"
    else
      let
        val sign = if den < 0 then ~1 else 1
        val num' = num * sign
        val den' = den * sign
        val g = gcd (num', den')
      in
        {num = num' div g, den = den' div g}
      end

  (* Coordinate vector of a rational weight against a given coroot list. *)
  fun coordsRatFromCoroots (coroots: int list list) (w: ratweight) : rat list =
    let
      val den = #den w
      val nums = #nums w
      val () = if den <= 0 then raise Fail "Coordinates.coordsRat: non-positive denom" else ()
    in
      List.map (fn cor => normRat {num = dot (cor, nums), den = den}) coroots
    end

  (* Coordinate vector against the simple coroots of `g`. *)
  fun coordsRat (g: AtlasFFI.group) (w: ratweight) : rat list =
    coordsRatFromCoroots (parseSimpleCorootsText (AtlasFFI.atlas_group_simple_coroots_text g)) w

  (* Rational `<=` comparison. *)
  fun leq (a: rat, b: rat) =
    #num a * #den b <= #num b * #den a

  (* Membership test for `[0,1]^r` in rational coordinates. *)
  fun in_fpp_rat (xs: rat list) : bool =
    let
      val zero = {num = 0, den = 1}
      val one = {num = 1, den = 1}
    in
      List.all (fn x => leq (zero, x) andalso leq (x, one)) xs
    end
end
