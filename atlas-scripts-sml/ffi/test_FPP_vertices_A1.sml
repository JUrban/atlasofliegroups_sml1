use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/FPP_vertices_fold.sml";

val g = AtlasFFI.atlas_group_new_simple (#"A", 1, #"s", 0);
val verts = FPP_vertices_fold.vertices g;
val () = if length verts = 2 then () else raise Fail ("A1: expected 2 vertices, got " ^ Int.toString (length verts));
val () = AtlasFFI.atlas_group_free g;
val () = print "OK\n";

