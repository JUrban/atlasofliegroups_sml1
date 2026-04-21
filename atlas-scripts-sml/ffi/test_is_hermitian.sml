use "atlas-scripts-sml/ffi/AtlasFFI.sml";

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);
val p = AtlasFFI.atlas_param_trivial g;
val _ = print ("is_hermitian(trivial) = " ^ Int.toString (AtlasFFI.atlas_param_is_hermitian p) ^ "\n");

val pt = AtlasFFI.atlas_param_twist p;
val _ = print ("equivalent(twist(trivial), trivial) = " ^ Int.toString (AtlasFFI.atlas_param_equivalent (pt, p)) ^ "\n");

val _ = AtlasFFI.atlas_param_free pt;
val _ = AtlasFFI.atlas_param_free p;
val _ = AtlasFFI.atlas_group_free g;

