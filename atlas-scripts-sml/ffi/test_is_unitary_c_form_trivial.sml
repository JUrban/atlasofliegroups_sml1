use "atlas-scripts-sml/ffi/AtlasFFI.sml";

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);
val p = AtlasFFI.atlas_param_trivial g;

val u = AtlasFFI.atlas_param_is_unitary_c_form p;
val _ = print ("is_unitary_c_form(trivial) = " ^ Int.toString u ^ "\n");

val _ = AtlasFFI.atlas_param_free p;
val _ = AtlasFFI.atlas_group_free g;

