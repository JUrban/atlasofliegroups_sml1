use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/KType.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);
val p = AtlasFFI.atlas_param_trivial g;
val t = KType.ofParam p;

val pol = KType.K_type_formula (t, 10);
val n = AtlasFFI.atlas_ktypepol_num_terms pol;
val _ = print ("terms=" ^ Int.toString n ^ "\n");
val _ = assert "nonempty" (n > 0);

val _ = AtlasFFI.atlas_ktypepol_free pol;
val _ = KType.free t;
val _ = AtlasFFI.atlas_param_free p;
val _ = AtlasFFI.atlas_group_free g;

val _ = print "ok\n";

