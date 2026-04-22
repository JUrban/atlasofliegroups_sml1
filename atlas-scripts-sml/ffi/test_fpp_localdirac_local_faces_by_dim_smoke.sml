use "atlas-scripts-sml/FPP_localDirac.sml";
use "atlas-scripts-sml/FPP_lambdas.sml";
use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(* Smoke test for `FPP_localDirac.local_faces_for_x_lambda_by_dim`. *)

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

val arr = FPP_localDirac.local_faces_for_x_lambda_by_dim (g, x, lambda);
val r = AtlasFFI.atlas_group_rank g;
val () = if Array.length arr = r + 1 then () else raise Fail "bad array length";

val total = List.foldl (op +) 0 (List.tabulate (r + 1, fn d => length (Array.sub (arr, d))));
val () = if total > 0 then () else raise Fail "expected some faces";

val () = AtlasFFI.atlas_group_free g;
val () = print "OK\n";

