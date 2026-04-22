use "atlas-scripts-sml/FPP_localDirac.sml";
use "atlas-scripts-sml/FPP_vertices.sml";
use "atlas-scripts-sml/FPP_lambdas.sml";
use "atlas-scripts-sml/VertexData.sml";
use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(* Smoke test for `FPP_localDirac.local_faces_for_x_lambda`. *)

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

val faces = FPP_localDirac.local_faces_for_x_lambda (g, x, lambda);
val () = if length faces > 0 then () else raise Fail "expected nonempty local face list";

val verts = FPP_vertices.vertices g;
val vd = VertexData.fromList verts;
val {Lvd, perm, mapAct, ...} = FPP_localDirac.localFD_Lvd_simple (g, x, lambda, vd);

fun memberInt (a: int, xs: int list) = List.exists (fn b => a = b) xs;

fun checkOne {gamma = _, global_face = gf, local_face = lf} =
  let
    val () = if length gf >= 1 andalso length gf <= 5 then () else raise Fail "unexpected global face arity"
    val () = if length lf >= 1 andalso length lf <= 5 then () else raise Fail "unexpected local face arity"
    val () = if lf = Basic.sort_u (op <=) lf then () else raise Fail "local face not sorted/unique"
    val () =
      List.app
        (fn i =>
          let
            val j = Array.sub (perm, i)
          in
            if j <> ~1 andalso memberInt (j, gf) then () else raise Fail "global face not stable under perm"
          end)
        gf
    val () =
      List.app
        (fn i =>
          let
            val k = Array.sub (mapAct, i)
          in
            if k >= 0 then () else raise Fail "mapAct undefined on face vertex"
          end)
        gf
    val nL = VertexData.size Lvd
    val () = List.app (fn k => if k >= 0 andalso k < nL then () else raise Fail "local index out of range") lf
  in
    ()
  end;

val () = List.app checkOne faces;

val () = AtlasFFI.atlas_group_free g;
val () = print "OK\n";

