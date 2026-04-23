use "atlas-scripts-sml/FPP_localDirac.sml";
use "atlas-scripts-sml/FPP_lambdas.sml";
use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(* Smoke test for `FPP_localDirac.unitary_local_faces_by_dim_exact_limit_ctx`.
   Uses an `x` known to have many lambdas, and a small face limit. *)

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);
val c = FPP_localDirac.create_ctx g;
val tbl = FPP_lambdas.FPP_lambdas_table g;

(* Note: `x=111` yields larger local face sets; we intentionally use a smaller
   case here to keep this smoke test fast. *)
val kgbSize = AtlasFFI.atlas_group_kgb_size g;
fun findX x =
  if x >= kgbSize then raise Fail "expected some x with nonempty lambda list"
  else let val ls = Array.sub (tbl, x) in if null ls then findX (x + 1) else (x, hd ls) end;
val (x, lambda) = findX 0;

val faces0 = FPP_localDirac.local_faces_for_x_lambda_by_dim_ctx (c, x, lambda);
val () = if length (Array.sub (faces0, 0)) > 0 then () else raise Fail "expected some local vertices";

val kept = FPP_localDirac.unitary_local_faces_by_dim_exact_limit_ctx (c, x, lambda, 5);
val () = if length (Array.sub (kept, 0)) > 0 then () else raise Fail "expected some unitary vertices";

val cache = BigUnitaryCache.create 256;
val kept2 = FPP_localDirac.unitary_local_faces_by_dim_exact_limit_ctx_cached cache (c, x, lambda, 5);
val () = if length (Array.sub (kept2, 0)) > 0 then () else raise Fail "expected some unitary vertices (cached)";
val () = BigUnitaryCache.freeAll cache;

val () = AtlasFFI.atlas_group_free g;
val () = print "OK\n";
