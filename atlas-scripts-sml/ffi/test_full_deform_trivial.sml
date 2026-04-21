use "atlas-scripts-sml/ffi/AtlasFFI.sml";

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);
val p = AtlasFFI.atlas_param_trivial g;

val _ = print ("is_standard(trivial) = " ^ Int.toString (AtlasFFI.atlas_param_is_standard p) ^ "\n");
val _ = print ("is_final(trivial) = " ^ Int.toString (AtlasFFI.atlas_param_is_final p) ^ "\n");
val _ = print ("lambda(trivial) = " ^ AtlasFFI.atlas_param_lambda_text p ^ "\n");
val _ = print ("nu(trivial) = " ^ AtlasFFI.atlas_param_nu_text p ^ "\n");

val kt = AtlasFFI.atlas_param_full_deform p;
val n = AtlasFFI.atlas_ktypepol_num_terms kt;
val _ = print ("full_deform(trivial) terms = " ^ Int.toString n ^ "\n");

val show = Int.min (n, 5);
fun loop i =
  if i >= show then ()
  else (print ("term[" ^ Int.toString i ^ "] = " ^ AtlasFFI.atlas_ktypepol_term_text (kt, i) ^ "\n"); loop (i + 1));
val _ = loop 0;

val _ = AtlasFFI.atlas_ktypepol_free kt;
val _ = AtlasFFI.atlas_param_free p;
val _ = AtlasFFI.atlas_group_free g;

val _ =
  if n < 0 then print ("C++ error: " ^ AtlasFFI.atlas_last_error () ^ "\n") else ();

