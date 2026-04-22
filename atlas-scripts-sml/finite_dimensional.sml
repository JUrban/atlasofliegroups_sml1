use "atlas-scripts-sml/RootDatum.sml";
use "atlas-scripts-sml/Lattice.sml";
use "atlas-scripts-sml/AllParameters.sml";
use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/finite_dimensional.sml

  Purpose
  - Partial SML translation of `atlas-scripts/finite_dimensional.at`, focused on
    Weyl’s dimension formula for algebraic finite-dimensional representations.

  Implemented functionality
  - `make_dominant(rd, lambda)` for rational weights using the RootDatum FFI
    wrapper `RootDatum.makeDominantRatWeightText`.
  - `dimension(rd, lambda)` computing the Weyl dimension formula as an
    `IntInf.int` (arbitrary precision) to avoid overflow.
  - Parameter-facing helpers (require an `AtlasFFI.group` handle):
      - highest weight from infinitesimal character
      - fundamental-weight coordinates of that highest weight
      - dimension of the corresponding finite-dimensional representation

  Atlas correspondence
  - In `finite_dimensional.at`:
      `dimension(rd, ratvec lambda)` computes:
        ∏_{a in poscoroots(rd)} ((lambda+rho)*a)/((rho)*a)
    after first making `lambda` dominant.
  - The Atlas script returns an `int` in the interpreter, which is effectively
    arbitrary precision. Here we return `IntInf.int`.

  Notes
  - This module is intentionally independent of “finite dimensional parameter”
    predicates like `is_finite_dimensional(p)` and tau-invariants; those require
    additional Atlas predicates not yet exposed via FFI.
*)

structure FiniteDimensional = struct
  type ratvec = Lattice.ratvec
  type group = AtlasFFI.group
  type param = AtlasFFI.param

  (* Parse a ratweight `den n1 ... nk` into a `ratvec`. *)
  fun parseRatvecText (s: string) : ratvec =
    let
      val {den, nums} = AllParameters.parseRatWeightText s
    in
      Lattice.ratvecNormalize {den = den, nums = nums}
    end

  fun ratvecToText (u: ratvec) : string =
    let
      val u = Lattice.ratvecNormalize u
      fun intToCText n =
        let
          val s = Int.toString n
        in
          if String.size s > 0 andalso String.sub (s, 0) = #"~" then
            "-" ^ String.extract (s, 1, NONE)
          else
            s
        end
    in
      String.concatWith " " (intToCText (#den u) :: List.map intToCText (#nums u))
    end

  (* Make a rational weight dominant for a root datum. *)
  fun make_dominant (rd: RootDatum.t, lambda: ratvec) : ratvec =
    parseRatvecText (RootDatum.makeDominantRatWeightText rd (ratvecToText lambda))

  (* Dot product pairing: (ratvec v) * (integral vector a) as a rational in lowest terms.
     Returned as `(num, den)` with `den > 0`, using `IntInf` for safety. *)
  fun dotRatvecInt (v: ratvec, a: int list) : IntInf.int * IntInf.int =
    let
      val v = Lattice.ratvecNormalize v
      val den = IntInf.fromInt (#den v)
      val nums = #nums v
      val () = if length nums = length a then () else raise Fail "FiniteDimensional.dotRatvecInt: length mismatch"
      val num =
        List.foldl IntInf.+ 0
          (ListPair.mapEq (fn (x, y) => IntInf.fromInt x * IntInf.fromInt y) (nums, a))
      fun gcd (x: IntInf.int, y: IntInf.int) : IntInf.int =
        let
          fun loop (u, 0) = IntInf.abs u
            | loop (u, w) = loop (w, IntInf.mod (u, w))
        in
          if x = 0 then IntInf.abs y else loop (IntInf.abs x, IntInf.abs y)
        end
      val g = gcd (num, den)
      val num = IntInf.div (num, g)
      val den = IntInf.div (den, g)
      val (num, den) = if den < 0 then (~num, ~den) else (num, den)
    in
      (num, den)
    end

  (* Add rational vectors. *)
  fun ratvecAdd (u: ratvec, v: ratvec) : ratvec =
    Lattice.ratvecSub (u, Lattice.ratvecScale (v, ~1, 1))

  (* Subtract rational vectors. *)
  fun ratvecSub (u: ratvec, v: ratvec) : ratvec = Lattice.ratvecSub (u, v)

  (* Multiply a rational accumulator `num/den` by a rational factor `a/b`,
     cancelling by gcd to keep sizes manageable. *)
  fun mulCancel (num: IntInf.int, den: IntInf.int, a: IntInf.int, b: IntInf.int) : IntInf.int * IntInf.int =
    let
      fun gcd (x: IntInf.int, y: IntInf.int) : IntInf.int =
        let
          fun loop (u, 0) = IntInf.abs u
            | loop (u, w) = loop (w, IntInf.mod (u, w))
        in
          if x = 0 then IntInf.abs y else loop (IntInf.abs x, IntInf.abs y)
        end
      val () = if den = 0 orelse b = 0 then raise Fail "FiniteDimensional.mulCancel: zero denom" else ()
      val g1 = gcd (a, b)
      val a = IntInf.div (a, g1)
      val b = IntInf.div (b, g1)
      val g2 = gcd (a, den)
      val a = IntInf.div (a, g2)
      val den = IntInf.div (den, g2)
      val g3 = gcd (num, b)
      val num = IntInf.div (num, g3)
      val b = IntInf.div (b, g3)
      val num = num * a
      val den = den * b
    in
      (num, den)
    end

  (* Weyl dimension formula (arbitrary precision result). *)
  fun dimension (rd: RootDatum.t, lambda: ratvec) : IntInf.int =
    let
      val lambda = make_dominant (rd, lambda)
      val rho = parseRatvecText (RootDatum.rhoText rd)
      val lambdaRho = ratvecAdd (lambda, rho)
      val posCoroots = RootDatum.posCorootsCols rd

      fun factor a =
        let
          val (n1, d1) = dotRatvecInt (lambdaRho, a)
          val (n0, d0) = dotRatvecInt (rho, a)
          val () =
            if n0 = 0 then raise Fail "FiniteDimensional.dimension: rho pairing zero" else ()
          (* (n1/d1) / (n0/d0) = (n1*d0)/(d1*n0) *)
          val num = n1 * d0
          val den = d1 * n0
        in
          (num, den)
        end

      fun loop ([], accNum, accDen) = (accNum, accDen)
        | loop (a :: rest, accNum, accDen) =
            let
              val (fNum, fDen) = factor a
              val (accNum, accDen) = mulCancel (accNum, accDen, fNum, fDen)
            in
              loop (rest, accNum, accDen)
            end

      val (num, den) = loop (posCoroots, 1, 1)
    in
      if den = 1 then num else raise Fail "FiniteDimensional.dimension: non-integral result"
    end

  (* Highest weight of a finite-dimensional parameter (ratvec), computed from:
       highest_weight = infinitesimal_character(p) - rho(root_datum(G)).

     Atlas correspondence
     - Mirrors `finite_dimensional.at`’s
         `highest_weight_finite_dimensional_ratvec(p)`
       but takes an explicit `group` handle.
  *)
  fun highest_weight_finite_dimensional_ratvec (g: group, p: param) : ratvec =
    let
      val rd = AtlasFFI.atlas_group_rootdatum_new g
      val () =
        if rd = Foreign.Memory.null then
          raise Fail ("FiniteDimensional.highest_weight_finite_dimensional_ratvec: rootdatum_new failed: " ^ AtlasFFI.atlas_last_error ())
        else
          ()
      val gamma = parseRatvecText (AtlasFFI.atlas_param_gamma_text p)
      val rho = parseRatvecText (RootDatum.rhoText rd)
      val hwt = ratvecSub (gamma, rho)
      val () = AtlasFFI.atlas_rootdatum_free rd
    in
      hwt
    end

  (* Evaluate a rational weight on a coroot vector, asserting integrality. *)
  fun evalIntegral (v: ratvec, a_v: int list) : int =
    let
      val v = Lattice.ratvecNormalize v
      val den = #den v
      val nums = #nums v
      val () = if length nums = length a_v then () else raise Fail "FiniteDimensional.evalIntegral: length mismatch"
      val num = List.foldl op+ 0 (ListPair.mapEq (op * ) (nums, a_v))
    in
      if num mod den = 0 then num div den else raise Fail "FiniteDimensional.evalIntegral: non-integral pairing"
    end

  (* Fundamental-weight coordinates of a weight `v` (assumed integral) in terms
     of the simple coroot basis:
       coordinates = (v * simple_coroots(rd)).ratvec_as_vec
     in `.at` terms.
  *)
  fun on_fundamental_weights (v: ratvec, rd: RootDatum.t) : int list =
    let
      val coroots = RootDatum.simpleCorootsCols rd
    in
      List.map (fn a_v => evalIntegral (v, a_v)) coroots
    end

  (* Fundamental-weight coordinates of the highest weight of a finite-dimensional parameter. *)
  fun fundamental_weight_coordinates (g: group, p: param) : int list =
    let
      val rd = AtlasFFI.atlas_group_rootdatum_new g
      val () =
        if rd = Foreign.Memory.null then
          raise Fail ("FiniteDimensional.fundamental_weight_coordinates: rootdatum_new failed: " ^ AtlasFFI.atlas_last_error ())
        else
          ()
      val hwt = highest_weight_finite_dimensional_ratvec (g, p)
      val coords = on_fundamental_weights (hwt, rd)
      val () = AtlasFFI.atlas_rootdatum_free rd
    in
      coords
    end

  (* Dimension of a finite-dimensional parameter (arbitrary precision). *)
  fun dimension_param (g: group, p: param) : IntInf.int =
    let
      val rd = AtlasFFI.atlas_group_rootdatum_new g
      val () =
        if rd = Foreign.Memory.null then
          raise Fail ("FiniteDimensional.dimension_param: rootdatum_new failed: " ^ AtlasFFI.atlas_last_error ())
        else
          ()
      val hwt = highest_weight_finite_dimensional_ratvec (g, p)
      val dim = dimension (rd, hwt)
      val () = AtlasFFI.atlas_rootdatum_free rd
    in
      dim
    end
end
