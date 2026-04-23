use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/AllParameters.sml";
use "atlas-scripts-sml/Lattice.sml";

(*
  Smoke test for `atlas_param_new_from_lambda_rho_gamma_text`.

  Strategy
  - Construct a known parameter `p` via an existing constructor.
  - Extract `x`, `lambda`, `rho`, and `gamma` from `p`.
  - Reconstruct `p2` via the new `sr_gamma`-based constructor using
    `lambda_rho = lambda - rho` and the same `gamma`.
  - Check that key printed invariants round-trip and (ideally) `param_equal`.
*)

fun assert msg b = if b then () else raise Fail ("assertion failed: " ^ msg);

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);
val p = AtlasFFI.atlas_param_trivial g;

val () = assert "param_trivial succeeded" (p <> Foreign.Memory.null);

val x = AtlasFFI.atlas_param_x p;
val lambdaTxt = AtlasFFI.atlas_param_lambda_text p;
val gammaTxt = AtlasFFI.atlas_param_gamma_text p;
val rhoTxt = AtlasFFI.atlas_group_rho_text g;

val lambda = AllParameters.parseRatWeightText lambdaTxt;
val gamma = AllParameters.parseRatWeightText gammaTxt;
val rho = AllParameters.parseRatWeightText rhoTxt;

val lambdaRhoRat = Lattice.ratvecSub (lambda, rho);
val lambdaRho =
  (case Lattice.ratvecToIntegral lambdaRhoRat of
     SOME v => v
   | NONE => raise Fail "expected lambda-rho integral");

val p2 =
  AtlasFFI.atlas_param_new_from_lambda_rho_gamma_text
    ( g
    , x
    , AllParameters.intsToCText lambdaRho
    , 1
    , AllParameters.intsToCText (#nums gamma)
    , #den gamma
    );

val () =
  if p2 = Foreign.Memory.null then
    raise Fail ("sr_gamma constructor failed: " ^ AtlasFFI.atlas_last_error ())
  else
    ();

val () = assert "lambda roundtrip" (AtlasFFI.atlas_param_lambda_text p2 = lambdaTxt);
val () = assert "gamma roundtrip" (AtlasFFI.atlas_param_gamma_text p2 = gammaTxt);
val () = assert "equal to original" (AtlasFFI.atlas_param_equal (p, p2) = 1);

val _ = AtlasFFI.atlas_param_free p2;
val _ = AtlasFFI.atlas_param_free p;
val _ = AtlasFFI.atlas_group_free g;

val _ = print "test_param_sr_gamma_smoke: ok\n";

