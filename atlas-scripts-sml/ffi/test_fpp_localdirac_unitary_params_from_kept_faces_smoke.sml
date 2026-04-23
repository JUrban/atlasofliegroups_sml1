use "atlas-scripts-sml/FPP_localDirac.sml";
use "atlas-scripts-sml/FPP_lambdas.sml";
use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(* Smoke test for `FPP_localDirac.unitary_params_from_unitary_faces_by_dim_exact_limit_ctx`. *)

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);
val c = FPP_localDirac.create_ctx g;
val tbl = FPP_lambdas.FPP_lambdas_table g;

(* Keep this smoke test small/fast; `x=111` works for heavier manual testing. *)
val kgbSize = AtlasFFI.atlas_group_kgb_size g;
fun findX x =
  if x >= kgbSize then raise Fail "expected some x with nonempty lambda list"
  else let val ls = Array.sub (tbl, x) in if null ls then findX (x + 1) else (x, hd ls) end;
val (x, lambda) = findX 0;

val ps = FPP_localDirac.unitary_params_from_unitary_faces_by_dim_exact_limit_ctx (c, x, lambda, 5);
val () = if length ps > 0 then () else raise Fail "expected some unitary params";
val () = List.app (fn p => if AtlasFFI.atlas_param_is_unitary p = 1 then () else raise Fail "nonunitary returned") ps;
val () = List.app AtlasFFI.atlas_param_free ps;

val () = AtlasFFI.atlas_group_free g;
val () = print "OK\n";
