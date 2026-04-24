use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/KType.sml";
use "atlas-scripts-sml/convert_c_form.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val g = AtlasFFI.atlas_group_new_simple (#"A", 1, #"s", 0);
val p = AtlasFFI.atlas_param_trivial g;
val t = KType.ofParam p;

val mu = Convert_c_form.muKType t;
val () = assert "mu denominator positive" (#den mu > 0);

val () = KType.free t;
val () = AtlasFFI.atlas_param_free p;
val () = AtlasFFI.atlas_group_free g;

val () = print "OK: ktype_mu_simple_smoke\n";

