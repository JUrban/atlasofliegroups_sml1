use "atlas-scripts-sml/FPP_localDirac.sml";
use "atlas-scripts-sml/FPP_lambdas.sml";
use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(* Smoke test for `FPP_localDirac.slice_hts_for_x_lambda_ctx` with the
   `pmax`/LKTs-based height schedule. *)

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);
val c = FPP_localDirac.create_ctx g;
val tbl = FPP_lambdas.FPP_lambdas_table g;

val kgbSize = AtlasFFI.atlas_group_kgb_size g;
fun findX x =
  if x >= kgbSize then raise Fail "expected some x with nonempty lambda list"
  else let val ls = Array.sub (tbl, x) in if null ls then findX (x + 1) else (x, hd ls) end;
val (x, lambda) = findX 0;

val () = (FPP_localDirac.ht_schedule_from_pmax_flag := true);
val hs = FPP_localDirac.slice_hts_for_x_lambda_ctx (c, x, lambda);

val rank = AtlasFFI.atlas_group_rank g;
val () = if length hs = rank + 1 then () else raise Fail "unexpected height-schedule length";
val () = if List.all (fn h => h >= 0) hs then () else raise Fail "expected nonnegative heights";

fun nondecreasing xs =
  case xs of
    [] => true
  | [_] => true
  | a :: b :: rest => a <= b andalso nondecreasing (b :: rest);
val () = if nondecreasing hs then () else raise Fail "expected nondecreasing heights";

val () = AtlasFFI.atlas_group_free g;
val () = print "OK\n";

