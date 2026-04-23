use "atlas-scripts-sml/FPP_localDirac.sml";
use "atlas-scripts-sml/FPP_lambdas.sml";
use "atlas-scripts-sml/ParamHash.sml";
use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(* Smoke test for ToHT-enabled `FPP_localDirac.local_test_GEO_hash2_limit_ctx`.
   The ToHT path must still return only exact-unitary parameters, and (for a
   fixed small face limit) should include the exact-baseline output. *)

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);
val c = FPP_localDirac.create_ctx g;
val tbl = FPP_lambdas.FPP_lambdas_table g;

val kgbSize = AtlasFFI.atlas_group_kgb_size g;
fun findX x =
  if x >= kgbSize then raise Fail "expected some x with nonempty lambda list"
  else let val ls = Array.sub (tbl, x) in if null ls then findX (x + 1) else (x, hd ls) end;
val (x, lambda) = findX 0;

val () = (FPP_localDirac.prefer_to_hts := true);
val () = (FPP_localDirac.ht_schedule_from_pmax_flag := false);
val () = (FPP_localDirac.ht_schedule_step := 5);

val uhash = ParamHash.create 64;
val psToHT = FPP_localDirac.local_test_GEO_hash2_limit_ctx (c, x, lambda, 5);
val () =
  List.app
    (fn p =>
       (if AtlasFFI.atlas_param_is_unitary p = 1 then () else raise Fail "ToHT path returned nonunitary";
        ignore (ParamHash.match uhash p);
        AtlasFFI.atlas_param_free p))
    psToHT;

val psExact = FPP_localDirac.local_test_GEO_hash2_exact_limit_ctx (c, x, lambda, 5);
val () =
  List.app
    (fn p =>
       (if ParamHash.contains uhash p then () else raise Fail "exact result missing from ToHT result";
        AtlasFFI.atlas_param_free p))
    psExact;

val () = ParamHash.freeAll uhash;
val () = AtlasFFI.atlas_group_free g;
val () = print "OK\n";

