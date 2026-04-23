use "atlas-scripts-sml/FPP_localDirac.sml";
use "atlas-scripts-sml/FPP_lambdas.sml";
use "atlas-scripts-sml/ParamHash.sml";
use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(* Smoke test for `FPP_localDirac.local_test_GEO_hash2_exact_limit_ctx`. *)

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);
val c = FPP_localDirac.create_ctx g;
val tbl = FPP_lambdas.FPP_lambdas_table g;

val x = 111;
val ls = Array.sub (tbl, x);
val () = if length ls > 0 then () else raise Fail "expected nonempty lambda list for x=111";
val lambda = hd ls;

val ps = FPP_localDirac.local_test_GEO_hash2_exact_limit_ctx (c, x, lambda, 30);
val () = if length ps > 0 then () else raise Fail "expected nonempty unitary list";
val () = List.app (fn p => if AtlasFFI.atlas_param_is_unitary p = 1 then () else raise Fail "nonunitary returned") ps;
val () = List.app AtlasFFI.atlas_param_free ps;

val uhash = ParamHash.create 64;
val added = FPP_localDirac.local_test_GEO_hash2_exact_into_hash_limit_ctx (c, x, lambda, 30, uhash);
val () = if added > 0 then () else raise Fail "expected some insertions";
val () = ParamHash.freeAll uhash;

val () = AtlasFFI.atlas_group_free g;
val () = print "OK\n";

