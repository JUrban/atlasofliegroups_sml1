use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/representations.sml";
use "atlas-scripts-sml/Lattice.sml";

fun assertTrue (b: bool, msg: string) = if b then () else raise Fail msg;

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);

val p0 = AtlasFFI.atlas_param_trivial g;
val p1 = Representations.finite_dimensional (g, Lattice.ratvecNormalize {den = 1, nums = [0, 0, 0, 0]});
val () = assertTrue (AtlasFFI.atlas_param_equivalent (p0, p1) = 1, "finite_dimensional(0) not equivalent to trivial");
val () = AtlasFFI.atlas_param_free p1;

val p2 = Representations.finite_dimensional_fundamental_weight_coordinates (g, [0, 0, 0, 0]);
val () = assertTrue (AtlasFFI.atlas_param_equivalent (p0, p2) = 1, "finite_dimensional_fw_coords(0) not equivalent to trivial");
val () = AtlasFFI.atlas_param_free p2;

val () = AtlasFFI.atlas_param_free p0;
val () = AtlasFFI.atlas_group_free g;
val () = print "OK\n";

