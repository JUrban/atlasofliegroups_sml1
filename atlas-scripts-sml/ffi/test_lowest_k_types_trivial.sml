use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/KType.sml";
use "atlas-scripts-sml/LowestKTypes.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);
val p = AtlasFFI.atlas_param_trivial g;

val lows = LowestKTypes.LKTs_param (g, p);
val _ = print ("LKTs(trivial) count=" ^ Int.toString (length lows) ^ "\n");
val _ = assert "nonempty" (length lows > 0);

val low = LowestKTypes.LKT_param (g, p);
val _ = print ("LKT(trivial).x=" ^ Int.toString (KType.x low) ^ "\n");

val _ = KType.free low;
val _ = List.app KType.free lows;
val _ = AtlasFFI.atlas_param_free p;
val _ = AtlasFFI.atlas_group_free g;

val _ = print "ok\n";

