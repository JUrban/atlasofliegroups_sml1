use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/KType.sml";
use "atlas-scripts-sml/K_highest_weights.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);
val p = AtlasFFI.atlas_param_trivial g;
val t = KType.ofParam p;

val ts = K_highest_weights.all_equal_dlambda_K_parameters (g, t);
val _ = print ("count=" ^ Int.toString (length ts) ^ "\n");
val _ = assert "nonempty" (length ts > 0);
val _ = assert "all final" (List.all KType.isFinal ts);
val _ = assert "all same x" (List.all (fn u => KType.x u = KType.x t) ts);

val _ = List.app KType.free ts;
val _ = KType.free t;
val _ = AtlasFFI.atlas_param_free p;
val _ = AtlasFFI.atlas_group_free g;

val _ = print "ok\n";

