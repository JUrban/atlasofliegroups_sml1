use "atlas-scripts-sml/ffi/AtlasFFI.sml";

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);
val p = AtlasFFI.atlas_param_trivial g;

val _ = print ("is_standard(trivial) = " ^ Int.toString (AtlasFFI.atlas_param_is_standard p) ^ "\n");
val _ = print ("is_final(trivial) = " ^ Int.toString (AtlasFFI.atlas_param_is_final p) ^ "\n");

val cf = AtlasFFI.atlas_param_c_form_irreducible p;
val n = AtlasFFI.atlas_ktypepol_num_terms cf;
val pure = AtlasFFI.atlas_ktypepol_is_typewise_pure cf;
val _ = print ("c_form_irreducible(trivial) terms = " ^ Int.toString n ^ "\n");
val _ = print ("typewise_pure = " ^ Int.toString pure ^ "\n");

val show = Int.min (n, 5);
fun loop i =
  if i >= show then ()
  else (print ("term[" ^ Int.toString i ^ "] = " ^ AtlasFFI.atlas_ktypepol_term_text (cf, i) ^ "\n"); loop (i + 1));
val _ = loop 0;

val _ = AtlasFFI.atlas_ktypepol_free cf;
val _ = AtlasFFI.atlas_param_free p;
val _ = AtlasFFI.atlas_group_free g;

val _ =
  if n < 0 then print ("C++ error: " ^ AtlasFFI.atlas_last_error () ^ "\n") else ();

