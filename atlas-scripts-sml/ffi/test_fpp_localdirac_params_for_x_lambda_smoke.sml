use "atlas-scripts-sml/FPP_localDirac.sml";
use "atlas-scripts-sml/FPP_lambdas.sml";
use "atlas-scripts-sml/WeylWord.sml";
use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(* Smoke test for `FPP_localDirac.gammas_for_x_lambda` / `params_for_x_lambda`. *)

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);
val lambdasByX = FPP_lambdas.FPP_lambdas_table g;
val kgbSize = AtlasFFI.atlas_group_kgb_size g;

fun findX x =
  if x >= kgbSize then
    raise Fail "expected some x with nonempty lambda table"
  else
    let
      val ls = Array.sub (lambdasByX, x)
    in
      if null ls then findX (x + 1) else (x, hd ls)
    end;

val (x, lambda) = findX 0;

val gammas = FPP_localDirac.gammas_for_x_lambda (g, x, lambda);
val () = if length gammas > 0 then () else raise Fail "expected nonempty gamma slice";

val ps = FPP_localDirac.params_for_x_lambda (g, x, lambda);
val () = if length ps > 0 then () else raise Fail "expected nonempty param slice";

val lambdaText = WeylWord.ratvecToText lambda;

fun checkOne p =
  let
    val () = if AtlasFFI.atlas_param_x p = x then () else raise Fail "wrong x in param slice"
    val () = if AtlasFFI.atlas_param_lambda_text p = lambdaText then () else raise Fail "wrong lambda in param slice"
    val () = if AtlasFFI.atlas_param_is_standard p = 1 andalso AtlasFFI.atlas_param_is_final p = 1 then () else raise Fail "expected standard+final"
  in
    ()
  end;

val () = List.app checkOne ps;
val () = List.app AtlasFFI.atlas_param_free ps;
val () = AtlasFFI.atlas_group_free g;

val () = print "OK\n";

