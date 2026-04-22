use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/KType.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);
val p = AtlasFFI.atlas_param_trivial g;

val t = KType.ofParam p;
val _ = print ("ktype.x = " ^ Int.toString (KType.x t) ^ "\n");
val _ = print ("ktype.lambda_rho = " ^ KType.lambdaRhoText t ^ "\n");
val _ = print ("ktype.is_final = " ^ Bool.toString (KType.isFinal t) ^ "\n");

val p2 = KType.parameter t;
val _ = assert "param(t) has nu=0" (AtlasFFI.atlas_param_nu_text p2 = "1 0 0 0 0");
val t2 = KType.ofParam p2;
val _ = assert "ktype roundtrip x" (KType.x t2 = KType.x t);
val _ = assert "ktype roundtrip lambda_rho" (KType.lambdaRhoText t2 = KType.lambdaRhoText t);

val _ = AtlasFFI.atlas_param_free p2;
val _ = KType.free t2;
val _ = KType.free t;
val _ = AtlasFFI.atlas_param_free p;
val _ = AtlasFFI.atlas_group_free g;

val _ = print "ok\n";
