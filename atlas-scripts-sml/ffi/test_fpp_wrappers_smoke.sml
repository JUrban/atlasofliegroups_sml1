use "atlas-scripts-sml/FPP_faces_geom.sml";
use "atlas-scripts-sml/FPP_vertices.sml";
use "atlas-scripts-sml/FPP_lambdas.sml";
use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(* Smoke test for compatibility wrapper modules:
   - ensure the `*_fold` implementations are reachable under the `.at`-style names
   - do a tiny amount of work to confirm the wrappers are wired correctly *)

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);
val rank = AtlasFFI.atlas_group_rank g;

val lines = FPP_faces_geom.FPP_lines g;
val () = if length lines = rank then () else raise Fail "expected #FPP_lines = rank";

val verts = FPP_vertices.vertices g;
val () = if length verts > 0 then () else raise Fail "expected nonempty vertex set";

val () = AtlasFFI.atlas_group_free g;
val () = print "OK\n";
