use "atlas-scripts-sml/FPP_localDirac.sml";
use "atlas-scripts-sml/FPP_lambdas.sml";
use "atlas-scripts-sml/WeylWord.sml";
use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(* Smoke test for `FPP_localDirac.params_for_local_faces_limit`. *)

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
val lambdaText = WeylWord.ratvecToText lambda;

val ps = FPP_localDirac.params_for_local_faces_limit (g, x, lambda, 10);
val () = if length ps > 0 then () else raise Fail "expected some final parameters from local faces";

fun checkOne p =
  let
    val () = if AtlasFFI.atlas_param_is_standard p = 1 andalso AtlasFFI.atlas_param_is_final p = 1 then () else raise Fail "expected standard+final"
    val () = if AtlasFFI.atlas_param_x p = x then () else raise Fail "wrong x"
    val () = if AtlasFFI.atlas_param_lambda_text p = lambdaText then () else raise Fail "wrong lambda"
  in
    ()
  end;

val () = List.app checkOne ps;
val () = List.app AtlasFFI.atlas_param_free ps;
val () = AtlasFFI.atlas_group_free g;

val () = print "OK\n";

