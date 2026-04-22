use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/KType.sml";
use "atlas-scripts-sml/K_highest_weights.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);
val p = AtlasFFI.atlas_param_trivial g;
val t = KType.ofParam p;
val t2 = KType.newFromXAndLambdaRhoText (g, KType.x t, KType.lambdaRhoText t);

val rs = K_highest_weights.reduce_K_parameters [t, t2];
val _ = print ("reduce_K_parameters count=" ^ Int.toString (length rs) ^ "\n");
val _ = assert "dedup" (length rs = 1);
val _ = assert "final" (List.all KType.isFinal rs);

val _ = List.app KType.free rs;
val _ = KType.free t2;
val _ = KType.free t;
val _ = AtlasFFI.atlas_param_free p;
val _ = AtlasFFI.atlas_group_free g;

val _ = print "ok\n";

