use "atlas-scripts-sml/FPP_localDirac.sml";
use "atlas-scripts-sml/FPP_lambdas.sml";
use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(* Smoke test for `.at`-name compatibility entry points in `FPP_localDirac`. *)

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);
val tbl = FPP_lambdas.FPP_lambdas_table g;
val kgbSize = AtlasFFI.atlas_group_kgb_size g;

fun findX x =
  if x >= kgbSize then raise Fail "expected some x with nonempty lambda list"
  else let val ls = Array.sub (tbl, x) in if null ls then findX (x + 1) else (x, hd ls) end;

val (x, lambda) = findX 0;

val ps1 = FPP_localDirac.local_test_GEO_hash2_limit (g, x, lambda, 2);
val ps2 = FPP_localDirac.local_test_GEO_hash_limit (g, x, lambda, 2);
val ps3 = FPP_localDirac.local_test_GEO_limit (g, x, lambda, 2);

val () = if length ps1 = length ps2 andalso length ps2 = length ps3 then () else raise Fail "compat entry points disagree";
val () = List.app AtlasFFI.atlas_param_free ps1;
val () = List.app AtlasFFI.atlas_param_free ps2;
val () = List.app AtlasFFI.atlas_param_free ps3;

val () = AtlasFFI.atlas_group_free g;
val () = print "OK\n";

