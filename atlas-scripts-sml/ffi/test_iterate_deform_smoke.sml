(* Smoke test for `atlas-scripts-sml/iterate_deform.sml` and `atlas_param_deform`. *)
use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/iterate_deform.sml";

val g = AtlasFFI.atlas_group_new_simple (#"A", 1, #"e", 0);
val () = if g = Foreign.Memory.null then raise Fail ("group_new_simple failed: " ^ AtlasFFI.atlas_last_error ()) else ();

val p = AtlasFFI.atlas_param_trivial g;
val () = if p = Foreign.Memory.null then raise Fail ("param_trivial failed: " ^ AtlasFFI.atlas_last_error ()) else ();

val pol = IterateDeform.deform p;
val () = print ("deform(trivial) terms: " ^ Int.toString (length (ParamPol.terms pol)) ^ "\n");

val () = (ParamPol.free pol; AtlasFFI.atlas_param_free p; AtlasFFI.atlas_group_free g);

