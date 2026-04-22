use "atlas-scripts-sml/FPP_localDirac.sml";
use "atlas-scripts-sml/FPP_lambdas.sml";
use "atlas-scripts-sml/ParamHash.sml";
use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(* Smoke test for `FPP_localDirac.create_ctx` and `_ctx` variants. *)

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);
val c = FPP_localDirac.create_ctx g;

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

val faces = FPP_localDirac.local_faces_for_x_lambda_ctx (c, x, lambda);
val () = if length faces > 0 then () else raise Fail "expected nonempty faces";

val ps = FPP_localDirac.params_for_local_faces_limit_ctx (c, x, lambda, 5);
val () = if length ps > 0 then () else raise Fail "expected some finals";
val () = List.app AtlasFFI.atlas_param_free ps;

val uhash = ParamHash.create 64;
val added = FPP_localDirac.local_test_GEO_simple_into_hash_limit_ctx (c, x, lambda, 5, uhash);
val () = if added >= 0 then () else raise Fail "unexpected negative added";
val () = ParamHash.freeAll uhash;

val gf = FPP_localDirac.global_face_of_gamma_ctx (c, hd (FPP_localDirac.gammas_for_x_lambda_ctx (c, x, lambda)));
val () = if length gf >= 1 andalso length gf <= 5 then () else raise Fail "unexpected face key arity";

val () = AtlasFFI.atlas_group_free g;
val () = print "OK\n";
