use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/FPP_faces_herm.sml";

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);
val cache = FPP_faces_herm.make_unitary_cache ();

val p = AtlasFFI.atlas_param_trivial g;
val u1 = FPP_faces_herm.is_unitary_hash_big_SIMPLE cache p;
val u2 = FPP_faces_herm.is_unitary_hash_big_SIMPLE cache p;
val () = print ("u1=" ^ Bool.toString u1 ^ " u2=" ^ Bool.toString u2 ^ "\n");

val () = AtlasFFI.atlas_param_free p;
val () = BigUnitaryCache.freeAll cache;
val () = AtlasFFI.atlas_group_free g;

