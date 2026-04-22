use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/KType.sml";
use "atlas-scripts-sml/K_highest_weights.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);
val p = AtlasFFI.atlas_param_trivial g;
val t = KType.ofParam p;

val _ = assert "trivial is split-spherical (split, x_open, even lambda_rho)" (K_highest_weights.is_split_spherical (g, t));

val tBad = KType.newFromXAndLambdaRhoText (g, 0, "0 0 0 0");
val _ = assert "wrong x not split-spherical" (not (K_highest_weights.is_split_spherical (g, tBad)));

val _ = KType.free tBad;
val _ = KType.free t;
val _ = AtlasFFI.atlas_param_free p;
val _ = AtlasFFI.atlas_group_free g;

val _ = print "ok\n";

