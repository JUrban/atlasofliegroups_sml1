use "atlas-scripts-sml/FPP_localDirac.sml";
use "atlas-scripts-sml/FPP_vertices.sml";
use "atlas-scripts-sml/FPP_lambdas.sml";
use "atlas-scripts-sml/VertexData.sml";
use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(* Smoke test for the baseline `FPP_localDirac.localFD_Lvd_simple`:
   - build the global folded-FPP vertex table for `F4_s`
   - pick a KGB element `x` with at least one admissible `lambda`
   - compute the induced affine involution permutation on vertices
   - check basic involution and index invariants *)

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);

val verts = FPP_vertices.vertices g;
val vd = VertexData.fromList verts;
val n = VertexData.size vd;
val () = if n > 0 then () else raise Fail "expected nonempty global vertex set";

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

val {Lvd, perm, pairs, mapAct} = FPP_localDirac.localFD_Lvd_simple (g, x, lambda, vd);

val () = if Array.length perm = n then () else raise Fail "perm length mismatch";
val () = if Array.length mapAct = n then () else raise Fail "mapAct length mismatch";

fun checkIdx i =
  let
    val j = Array.sub (perm, i)
    val k = Array.sub (mapAct, i)
  in
    if j = ~1 then ()
    else if j < 0 orelse j >= n then raise Fail "perm out of range"
    else if Array.sub (perm, j) <> i then raise Fail "perm not an involution on its domain"
    else ();
    if k = ~1 then ()
    else if k < 0 orelse k >= VertexData.size Lvd then raise Fail "mapAct out of range"
    else ()
  end;

val () = List.app checkIdx (List.tabulate (n, fn i => i));

fun checkPair (i, j) =
  let
    val () = if i < j then () else raise Fail "expected representative i<j"
    val () = if Array.sub (perm, i) = j andalso Array.sub (perm, j) = i then () else raise Fail "pair not transposed in perm"
    val ki = Array.sub (mapAct, i)
    val kj = Array.sub (mapAct, j)
  in
    if ki = kj andalso ki <> ~1 then () else raise Fail "paired vertices should map to same local index"
  end;

val () = List.app checkPair pairs;
val () = if VertexData.size Lvd > 0 then () else raise Fail "expected nonempty local vertex set";

val () = AtlasFFI.atlas_group_free g;
val () = print "OK\n";

