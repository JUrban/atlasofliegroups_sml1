use "atlas-scripts-sml/ffi/AtlasFFI.sml";

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);
val k = AtlasFFI.atlas_group_kgb_size g;
val _ = print ("KGB_size(F4_s) = " ^ Int.toString k ^ "\n");
val _ = print ("numRealForms(F4_s) = " ^ Int.toString (AtlasFFI.atlas_group_num_real_forms g) ^ "\n");
val _ = AtlasFFI.atlas_group_free g;

val _ =
  if k < 0
  then print ("C++ error: " ^ AtlasFFI.atlas_last_error () ^ "\n")
  else ();
