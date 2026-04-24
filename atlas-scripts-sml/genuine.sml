use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/KTypePol.sml";
use "atlas-scripts-sml/ParamPol.sml";
use "atlas-scripts-sml/Split.sml";
use "atlas-scripts-sml/Hermitian.sml";

(*
  File: atlas-scripts-sml/genuine.sml

  Purpose
  - Partial Standard ML translation of `atlas-scripts/genuine.at`.
  - Focuses on the sign/positivity utilities for split-integer coefficients
    that are broadly useful when working with virtual characters represented
    as `KTypePol` and `ParamPol`.

  Background: split integers
  - Atlas represents coefficients in “split integers” `a + s*b` with `s^2 = 1`.
  - The `.at` script tests positivity by checking `a >= 0` AND `b >= 0`
    coefficient-wise.

  What is ported
  - For `KTypePol`:
      - `is_positive_ktypepol`
      - `coeff_normalize_ktypepol`
      - `is_positive_or_negative_ktypepol`
  - For `ParamPol`:
      - `is_positive_parampol`
      - `coeff_normalize_parampol`
      - `is_positive_or_negative_parampol`
      - `test_positive_parampol` / `test_positive_or_negative_parampol`
  - Hermitian form alias:
      - `hermitian_form_irreducible_normalized` (currently identical to
        `Hermitian.hermitian_form_irreducible`)

  Not yet ported
  - The “genuine” tests that depend on branching (`branch`, `branch_std`) are
    not available in the current Poly/ML+FFI layer, so functions such as
    `is_genuine` are not implemented here.

  Ownership and performance notes
  - `coeff_normalize_ktypepol` returns a fresh `KTypePol` handle; caller must
    free it using `KTypePol.free`.
  - `coeff_normalize_parampol` returns a fresh `ParamPol.t` (owning cloned
    parameters); caller must free it using `ParamPol.free`.
  - The `_or_negative` predicates are implemented without allocating new
    polynomials (unlike the `.at` formulation), by checking either all
    coefficients are nonnegative or all are nonpositive depending on the
    sign-normalization rule.
*)

structure Genuine = struct
  type ktypepol = AtlasFFI.ktypepol
  type parampol = ParamPol.t
  type param = AtlasFFI.param

  fun fail where' msg = raise Fail ("Genuine." ^ where' ^ ": " ^ msg)

  (* Parse the leading `(e,s,...)` from `atlas_ktypepol_term_text`. *)
  fun parseES (s: string) : int * int =
    let
      fun toInt tok =
        case Int.fromString tok of
          SOME n => n
        | NONE => fail "parseES" ("bad int token: " ^ tok)
      val ns = List.map toInt (String.tokens Char.isSpace s)
    in
      case ns of
        e :: s :: _ => (e, s)
      | _ => fail "parseES" "truncated KTypePol term text"
    end

  fun splitNonnegInt (e: int, s: int) : bool = (e >= 0 andalso s >= 0)
  fun splitNonposInt (e: int, s: int) : bool = (e <= 0 andalso s <= 0)

  fun splitNonneg (w: Split.t) : bool =
    Split.int_part w >= 0 andalso Split.s_part w >= 0

  fun splitNonpos (w: Split.t) : bool =
    Split.int_part w <= 0 andalso Split.s_part w <= 0

  (* `.at`: `is_positive (KTypePol P)`. *)
  fun is_positive_ktypepol (pol: ktypepol, rank: int) : bool =
    List.all (fn t => splitNonnegInt (#e t, #s t)) (KTypePol.terms (pol, rank))

  (* `.at`: `is_positive (ParamPol P)`. *)
  fun is_positive_parampol (poly: parampol) : bool =
    List.all (fn (c, _) => splitNonneg c) (ParamPol.terms poly)

  (* Decide whether a split integer `a + s*b` is “negative” for normalization
     purposes:
       negate if `a < 0` OR (`a = 0` AND `b < 0`). *)
  fun coefNeedsNegateInt (e: int, s: int) : bool = (e < 0 orelse (e = 0 andalso s < 0))
  fun coefNeedsNegate (w: Split.t) : bool =
    (Split.int_part w < 0 orelse (Split.int_part w = 0 andalso Split.s_part w < 0))

  (* `.at`: `coeff_normalize(KTypePol P)`.

     Returns a fresh `KTypePol` handle:
     - if `P` is empty, returns a clone
     - otherwise, returns either `clone(P)` or `(-1)*P` depending on the first term’s coefficient
  *)
  fun coeff_normalize_ktypepol (pol: ktypepol) : ktypepol =
    let
      val n = AtlasFFI.atlas_ktypepol_num_terms pol
      val () =
        if n < 0 then fail "coeff_normalize_ktypepol" ("num_terms failed: " ^ AtlasFFI.atlas_last_error ()) else ()
    in
      if n = 0 then
        KTypePol.clone pol
      else
        let
          val txt = AtlasFFI.atlas_ktypepol_term_text (pol, 0)
          val () =
            if txt = "-1" then fail "coeff_normalize_ktypepol" ("term_text failed: " ^ AtlasFFI.atlas_last_error ()) else ()
          val (e0, s0) = parseES txt
        in
          if coefNeedsNegateInt (e0, s0) then KTypePol.scaleSplit (pol, ~1, 0) else KTypePol.clone pol
        end
    end

  (* `.at`: `coeff_normalize(ParamPol P)`.

     Returns a fresh polynomial owning cloned parameters. *)
  fun coeff_normalize_parampol (poly: parampol) : parampol =
    (case ParamPol.terms poly of
       [] => ParamPol.create ()
     | (c0, _) :: _ =>
         let
           val out = ParamPol.clone poly
           val () = if coefNeedsNegate c0 then ParamPol.scaleInPlace (out, Split.minus_one) else ()
         in
           out
         end)

  (* `.at`: `is_positive_or_negative(P) = is_positive(coeff_normalize(P))`.

     Implemented allocation-free by checking that all coefficients have the
     same (normalized) sign. *)
  fun is_positive_or_negative_parampol (poly: parampol) : bool =
    (case ParamPol.terms poly of
       [] => true
     | (c0, _) :: _ =>
         if coefNeedsNegate c0 then
           List.all (fn (c, _) => splitNonpos c) (ParamPol.terms poly)
         else
           List.all (fn (c, _) => splitNonneg c) (ParamPol.terms poly))

  fun is_positive_or_negative_ktypepol (pol: ktypepol, rank: int) : bool =
    let
      val n = AtlasFFI.atlas_ktypepol_num_terms pol
      val () =
        if n < 0 then fail "is_positive_or_negative_ktypepol" ("num_terms failed: " ^ AtlasFFI.atlas_last_error ()) else ()
      fun termES i =
        let
          val txt = AtlasFFI.atlas_ktypepol_term_text (pol, i)
          val () =
            if txt = "-1" then fail "is_positive_or_negative_ktypepol" ("term_text failed: " ^ AtlasFFI.atlas_last_error ()) else ()
        in
          parseES txt
        end
    in
      if n = 0 then
        true
      else
        let
          val (e0, s0) = termES 0
          fun ok i =
            let
              val (e, s) = termES i
            in
              if coefNeedsNegateInt (e0, s0) then splitNonposInt (e, s) else splitNonnegInt (e, s)
            end
        in
          List.all ok (List.tabulate (n, fn i => i))
        end
    end

  (* `.at`: `test_positive(ParamPol P)` but returns a `ParamPol` of the “bad”
     terms (those with a<0 or b<0), and a boolean that is `true` iff no bad terms.

     Ownership
     - Returned polynomial owns cloned parameters; caller must free it.
  *)
  fun test_positive_parampol (poly: parampol) : parampol * bool =
    let
      val bad = ParamPol.create ()
      fun addIfBad (c, p) =
        if Split.int_part c < 0 orelse Split.s_part c < 0 then
          ParamPol.addTermClone (bad, c, p)
        else
          ()
      val () = List.app addIfBad (ParamPol.terms poly)
    in
      (bad, ParamPol.isEmpty bad)
    end

  fun test_positive_or_negative_parampol (poly: parampol) : parampol * bool =
    test_positive_parampol (coeff_normalize_parampol poly)

  (* `.at` convenience name:
       hermitian_form_irreducible_normalized = hermitian_form_irreducible@Param
     In this port there is currently only one implementation, so this is an alias. *)
  val hermitian_form_irreducible_normalized = Hermitian.hermitian_form_irreducible

  (*
    “Genuineness” predicates

    Atlas meaning
    - In `genuine.at`, a `KTypePol P` is interpreted as a virtual sum of
      standard representations of `K` (“standardrepKs”).
    - `branch(P,height_bound)` rewrites it (up to the given height cutoff) as a
      sum of *actual* K-types.
    - `is_genuine(P,height_bound)` holds iff all coefficients in that branched
      expansion have nonnegative integer and `s` parts.

    SML port
    - We delegate branching to the C++ `Rep_context::branch` via `KTypePol.branch`.
    - Because a `KTypePol` handle does not expose its ambient group/rank on the
      SML side, callers must supply the group handle `g` so we can decode term
      data and test coefficient signs.
  *)

  fun is_genuine (g: AtlasFFI.group, pol: ktypepol, height_bound: int) : bool =
    let
      val bran = KTypePol.branch (pol, height_bound)
      val r = AtlasFFI.atlas_group_rank g
      val ok = is_positive_ktypepol (bran, r)
      val () = KTypePol.free bran
    in
      ok
    end

  fun test_genuine (g: AtlasFFI.group, pol: ktypepol, height_bound: int) : bool =
    is_genuine (g, pol, height_bound)

  fun is_hermitian_form_irreducible_genuine (g: AtlasFFI.group, p: param, height_bound: int) : bool =
    let
      val hf = hermitian_form_irreducible_normalized p
      val ok =
        (is_genuine (g, hf, height_bound)
         handle e => (KTypePol.free hf; raise e))
      val () = KTypePol.free hf
    in
      ok
    end

  fun test_hermitian_form_irreducible_genuine (g: AtlasFFI.group, p: param, height_bound: int) : bool =
    is_hermitian_form_irreducible_genuine (g, p, height_bound)
end
