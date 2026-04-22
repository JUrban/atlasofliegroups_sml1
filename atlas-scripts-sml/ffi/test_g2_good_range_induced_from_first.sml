use "atlas-scripts-sml/G2_unitary_dual.sml";

fun assertTrue msg b = if b then () else raise Fail ("assertTrue: " ^ msg);

val g = AtlasFFI.atlas_group_new_simple (#"G", 2, #"s", 0);

val ps = G2_unitary_dual.p_s g (G2_unitary_dual.ratOfInt 3, {num = ~1, den = 2});
val p = hd ps;

val unitary = AtlasFFI.atlas_param_is_unitary p = 1;
val coords = G2_unitary_dual.coords_infchar g p;
val inFPP = G2_unitary_dual.in_fpp_rat coords;
val info = AtlasFFI.atlas_param_good_range_induced_from_first_text p;

val () = assertTrue "expected unitary" unitary;
val () = assertTrue "expected outside FPP" (not inFPP);
val () = assertTrue "expected induced info prefix" (String.isPrefix "1|0|1|" info);

val () = List.app AtlasFFI.atlas_param_free ps;
val () = AtlasFFI.atlas_group_free g;

val () = print ("OK: " ^ info ^ "\n");

