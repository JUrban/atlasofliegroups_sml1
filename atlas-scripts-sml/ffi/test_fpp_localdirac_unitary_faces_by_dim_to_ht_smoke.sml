use "atlas-scripts-sml/FPP_localDirac.sml";
use "atlas-scripts-sml/FPP_lambdas.sml";
use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(* Smoke test for `FPP_localDirac.unitary_local_faces_by_dim_to_ht_limit_ctx`.
   This is a pruning pass, so it should (conservatively) keep at least the
   exact-unitary vertices in dimension 0. *)

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);
val c = FPP_localDirac.create_ctx g;
val tbl = FPP_lambdas.FPP_lambdas_table g;

val kgbSize = AtlasFFI.atlas_group_kgb_size g;
fun findX x =
  if x >= kgbSize then raise Fail "expected some x with nonempty lambda list"
  else let val ls = Array.sub (tbl, x) in if null ls then findX (x + 1) else (x, hd ls) end;
val (x, lambda) = findX 0;

(* Use a tiny face limit to keep this test quick. *)
val keptExact = FPP_localDirac.unitary_local_faces_by_dim_exact_limit_ctx (c, x, lambda, 5);
val keptToHT = FPP_localDirac.unitary_local_faces_by_dim_to_ht_limit_ctx (c, x, lambda, 5);

val ex0 = Array.sub (keptExact, 0);
val th0 = Array.sub (keptToHT, 0);

fun mem xs x = List.exists (fn y => y = x) xs;
val () = if List.all (fn f => mem th0 f) ex0 then () else raise Fail "expected ToHT vertex set to include exact vertex set";

val () = AtlasFFI.atlas_group_free g;
val () = print "OK\n";

