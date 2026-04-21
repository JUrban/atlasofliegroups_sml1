use "atlas-scripts-sml/ffi/AtlasFFI.sml";

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);
val p = AtlasFFI.atlas_param_trivial g;
val h = AtlasFFI.atlas_param_height p;

val _ = print ("trivial(F4_s) height = " ^ Int.toString h ^ "\n");

val _ = AtlasFFI.atlas_param_free p;
val _ = AtlasFFI.atlas_group_free g;

val _ =
  if h < 0
  then print ("C++ error: " ^ AtlasFFI.atlas_last_error () ^ "\n")
  else ();

