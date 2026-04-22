use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/Weylgroup.sml";
use "atlas-scripts-sml/WeylWord.sml";
use "atlas-scripts-sml/representations.sml";

(*
  File: atlas-scripts-sml/ffi/test_Weylgroup_from_no_Cminus_param_smoke.sml

  Purpose
  - Smoke test for `Weylgroup.from_no_Cminus_param`.

  What it checks
  - Transporting a parameter to `x0` and back using the witness word reproduces
    the original `(x,lambda,nu)` (up to normalization).

  Usage
  - `poly -q < atlas-scripts-sml/ffi/test_Weylgroup_from_no_Cminus_param_smoke.sml`
*)

fun normaliseOrFail (p: AtlasFFI.param) : AtlasFFI.param =
  let
    val q = AtlasFFI.atlas_param_normalise p
  in
    if q = Foreign.Memory.null then raise Fail (AtlasFFI.atlas_last_error ()) else q
  end

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0)
val () = if g = Foreign.Memory.null then raise Fail (AtlasFFI.atlas_last_error ()) else ()

(* Pick a parameter whose KGB element is not already "simple". *)
val p0 = Representations.large_fundamental_series_default g
val p = normaliseOrFail p0
val () = AtlasFFI.atlas_param_free p0

val x = AtlasFFI.atlas_param_x p
val (w, q) = Weylgroup.from_no_Cminus_param (g, p)

(* Move back by applying `w` to `q`. *)
val x0 = AtlasFFI.atlas_param_x q
val lamQ = WeylWord.actRatvec (g, w, Lattice.ratvecNormalize (AllParameters.parseRatWeightText (AtlasFFI.atlas_param_lambda_text q)))
val nuQ = WeylWord.actRatvec (g, w, Lattice.ratvecNormalize (AllParameters.parseRatWeightText (AtlasFFI.atlas_param_nu_text q)))
val xBack = WeylWord.kgbCrossLeft (g, w, x0)
val pBack0 = Representations.parameter (g, xBack, lamQ, nuQ)
val pBack = normaliseOrFail pBack0
val () = AtlasFFI.atlas_param_free pBack0

val pN = normaliseOrFail p
val () = AtlasFFI.atlas_param_free p

val () = if AtlasFFI.atlas_param_equal (pN, pBack) = 1 then () else raise Fail "transport-back parameter mismatch"

val () = AtlasFFI.atlas_param_free pN
val () = AtlasFFI.atlas_param_free pBack
val () = AtlasFFI.atlas_param_free q
val () = AtlasFFI.atlas_group_free g

val () = TextIO.print "OK\n"

