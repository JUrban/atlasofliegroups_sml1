use "atlas-scripts-sml/FPP_localDirac.sml";
use "atlas-scripts-sml/FPP_lambdas.sml";
use "atlas-scripts-sml/ParamHash.sml";
use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(* Smoke test for `FPP_localDirac.add_unitary_from_local_faces_limit`. *)

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

val uhash = ParamHash.create 256;
val added = FPP_localDirac.add_unitary_from_local_faces_limit (g, x, lambda, 10, uhash);
val () = if added >= 0 andalso ParamHash.size uhash >= added then () else raise Fail "bad insertion count";
val () = if ParamHash.size uhash > 0 then () else raise Fail "expected some unitary insertions (limit=10)";

val () = ParamHash.freeAll uhash;
val () = AtlasFFI.atlas_group_free g;
val () = print "OK\n";

