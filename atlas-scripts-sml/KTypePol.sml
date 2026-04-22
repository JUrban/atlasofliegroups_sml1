use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/KTypePol.sml

  Purpose
  - Decode `KTypePol` (polynomials in K-types) returned by Atlas routines such as
    `full_deform` and `K_type_formula`.

  Ownership
  - `type ktypepol = AtlasFFI.ktypepol` is an opaque handle; free with `free`.

  Atlas correspondence
  - The `.at` language represents coefficients as “split integers” `a + s*b`.
    In the FFI term text, we expose these components as `e` and `s`:
      - `e`: the integer part `a`
      - `s`: the `s`-part coefficient `b`
  - The helper `purityCounts` mirrors `purity(KTypePol)` from `atlas-scripts/basic.at`:
      returns a triple `(num_int,num_s,num_mixed)` counting coefficients of the
      three forms `a`, `s*b`, and `a+s*b` with `a,b != 0`.
*)
structure KTypePol = struct
  type ktypepol = AtlasFFI.ktypepol

  type term = {e: int, s: int, x: int, height: int, lambdaRho: int list}

  (* Parse whitespace-separated integers. *)
  fun parseInts s =
    let
      fun toInt tok =
        case Int.fromString tok of
          SOME n => n
        | NONE => raise Fail ("KTypePol: bad int token: " ^ tok)
    in
      List.map toInt (String.tokens Char.isSpace s)
    end

  (* Extract polynomial terms, decoding `atlas_ktypepol_term_text` output.
     `rank` is the ambient group rank (controls lambda_rho vector length). *)
  fun terms (pol: ktypepol, rank: int) : term list =
    let
      val n = AtlasFFI.atlas_ktypepol_num_terms pol
      val () = if n < 0 then raise Fail ("KTypePol.terms: bad poly: " ^ AtlasFFI.atlas_last_error ()) else ()

      fun one i =
        let
          val s = AtlasFFI.atlas_ktypepol_term_text (pol, i)
          val () = if s = "-1" then raise Fail ("KTypePol.terms: term_text failed: " ^ AtlasFFI.atlas_last_error ()) else ()
          val ns = parseInts s
        in
          case ns of
            e :: s :: x :: height :: rest =>
              if length rest <> rank then
                raise Fail "KTypePol.terms: wrong lambda_rho length"
              else
                {e = e, s = s, x = x, height = height, lambdaRho = rest}
          | _ => raise Fail "KTypePol.terms: truncated term"
        end
    in
      List.tabulate (n, one)
    end

  (* Free a KTypePol handle. *)
  fun free (pol: ktypepol) : unit = AtlasFFI.atlas_ktypepol_free pol

  (* Clone a polynomial (new owned handle). *)
  fun clone (pol: ktypepol) : ktypepol =
    let
      val q = AtlasFFI.atlas_ktypepol_clone pol
    in
      if q = Foreign.Memory.null then
        raise Fail ("KTypePol.clone: failed: " ^ AtlasFFI.atlas_last_error ())
      else
        q
    end

  (* Add two polynomials (new owned handle). *)
  fun add (a: ktypepol, b: ktypepol) : ktypepol =
    let
      val q = AtlasFFI.atlas_ktypepol_add (a, b)
    in
      if q = Foreign.Memory.null then
        raise Fail ("KTypePol.add: failed: " ^ AtlasFFI.atlas_last_error ())
      else
        q
    end

  (* Scale by a split integer `e + s*s` (new owned handle). *)
  fun scaleSplit (pol: ktypepol, e: int, s: int) : ktypepol =
    let
      val q = AtlasFFI.atlas_ktypepol_scale_split (pol, e, s)
    in
      if q = Foreign.Memory.null then
        raise Fail ("KTypePol.scaleSplit: failed: " ^ AtlasFFI.atlas_last_error ())
      else
        q
    end

  (* Test purity of a single split coefficient `(e,s)` as in `basic.at`:
     “pure” means `e=0` or `s=0`. *)
  fun coefIsPure (t: term) : bool = #e t = 0 orelse #s t = 0

  (* Count coefficient purity classes, mirroring `purity(KTypePol)` in `basic.at`.
     Returns `(num_int,num_s,num_mixed)` where:
       - int: `s=0`
       - s:   `e=0`
       - mixed: otherwise *)
  fun purityCounts (pol: ktypepol, rank: int) : int * int * int =
    let
      fun step (t: term, (nInt, nS, nMix)) =
        if #s t = 0 then (nInt + 1, nS, nMix)
        else if #e t = 0 then (nInt, nS + 1, nMix)
        else (nInt, nS, nMix + 1)
    in
      List.foldl step (0, 0, 0) (terms (pol, rank))
    end

  (* Coefficient-wise purity test `is_pure(KTypePol)` from `basic.at`. *)
  fun isPure (pol: ktypepol, rank: int) : bool =
    List.all coefIsPure (terms (pol, rank))

  (* Stronger “module purity” test from `basic.at`: pure if all coefficients
     are integers (`s=0` for all terms) OR all are `s`-multiples (`e=0` for all). *)
  fun isPureModule (pol: ktypepol, rank: int) : bool =
    let
      val ts = terms (pol, rank)
      val allInt = List.all (fn t => #s t = 0) ts
      val allS = List.all (fn t => #e t = 0) ts
    in
      allInt orelse allS
    end

  fun purityString (pol: ktypepol, rank: int) : string =
    let
      val (a, b, c) = purityCounts (pol, rank)
    in
      "(" ^ Int.toString a ^ "," ^ Int.toString b ^ "," ^ Int.toString c ^ ")"
    end

  (* Truncate a `KTypePol` by term height, analogous to `to_ht(KTypePol,HT)` in
     the `.at` scripts. Returns a new owned handle (caller must free it). *)
  fun toHT (pol: ktypepol, cutoff: int) : ktypepol =
    let
      val q = AtlasFFI.atlas_ktypepol_to_ht (pol, cutoff)
    in
      if q = Foreign.Memory.null then
        raise Fail ("KTypePol.toHT: failed: " ^ AtlasFFI.atlas_last_error ())
      else
        q
    end
end
