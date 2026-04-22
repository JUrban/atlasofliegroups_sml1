use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/FPP_vertices_fold.sml";
use "atlas-scripts-sml/VertexData.sml";
use "atlas-scripts-sml/Lattice.sml";

val g = AtlasFFI.atlas_group_new_simple (#"A", 1, #"s", 0);
val verts = FPP_vertices_fold.vertices g;
val vd = VertexData.fromList verts;

val () = if VertexData.size vd = 2 then () else raise Fail "expected 2 vertices";

val v0 = List.nth (verts, 0);
val v1 = List.nth (verts, 1);

val i0 = VertexData.lookupExn (vd, v0);
val i1 = VertexData.lookupExn (vd, v1);
val () = if i0 <> i1 then () else raise Fail "distinct verts must have distinct indices";

val b = VertexData.face_bary (vd, [i0, i1]);
val () =
  if b = v0 orelse b = v1 then
    raise Fail "barycenter should not equal an endpoint"
  else
    ();
val () =
  (case VertexData.lookup (vd, b) of
     NONE => ()
   | SOME _ => raise Fail "barycenter should not be a vertex");

val () = AtlasFFI.atlas_group_free g;
val () = print "OK\n";
