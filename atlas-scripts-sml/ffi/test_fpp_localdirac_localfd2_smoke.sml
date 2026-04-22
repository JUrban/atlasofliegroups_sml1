use "atlas-scripts-sml/FPP_localDirac.sml";
use "atlas-scripts-sml/FPP_lambdas.sml";
use "atlas-scripts-sml/VertexData.sml";
use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(* Smoke test for `FPP_localDirac.localFD_Lvd2_simple`. *)

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

val (Lvd, (fixed, pairs), mapAct) = FPP_localDirac.localFD_Lvd2_simple (g, x, lambda);
val () = if VertexData.size Lvd > 0 then () else raise Fail "expected nonempty local vertex list";
val () = if Array.length mapAct > 0 then () else raise Fail "expected nonempty mapAct";
val () = if List.all (fn i => i >= 0) fixed then () else raise Fail "bad fixed index";
val () = if List.all (fn (i, j) => i < j) pairs then () else raise Fail "expected i<j pairs";

val () = AtlasFFI.atlas_group_free g;
val () = print "OK\n";

