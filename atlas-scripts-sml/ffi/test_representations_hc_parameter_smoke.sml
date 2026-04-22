use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/representations.sml";
use "atlas-scripts-sml/Lattice.sml";
use "atlas-scripts-sml/AllParameters.sml";

(*
  File: atlas-scripts-sml/ffi/test_representations_hc_parameter_smoke.sml

  Purpose
  - Smoke test for `Representations.hc_parameter` / `hc_parameter_at_xb`.

  What it checks
  - For the simple case where `x(p) = x_b`, `hc_parameter_at_xb` returns
    `lambda(p)` (since the witness word is the identity).

  Usage
  - `poly -q < atlas-scripts-sml/ffi/test_representations_hc_parameter_smoke.sml`
*)

fun eqRatvec (u: Lattice.ratvec, v: Lattice.ratvec) : bool =
  let
    val u = Lattice.ratvecNormalize u
    val v = Lattice.ratvecNormalize v
  in
    #den u = #den v andalso #nums u = #nums v
  end

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0)
val () = if g = Foreign.Memory.null then raise Fail (AtlasFFI.atlas_last_error ()) else ()

val lam = Representations.rho g
val p = Representations.discrete_series_at_x (g, 0, lam)
val () = if p = Foreign.Memory.null then raise Fail "discrete_series_at_x returned null" else ()

val xP = AtlasFFI.atlas_param_x p
val () = if xP = 0 then () else raise Fail "unexpected: x(p) <> 0 for dominant lambda"

val hc = Representations.hc_parameter_at_xb (g, p, 0)
val lamP = Lattice.ratvecNormalize (AllParameters.parseRatWeightText (AtlasFFI.atlas_param_lambda_text p))
val () = if eqRatvec (hc, lamP) then () else raise Fail "hc_parameter mismatch"

val () = AtlasFFI.atlas_param_free p
val () = AtlasFFI.atlas_group_free g
val () = TextIO.print "OK\n"

