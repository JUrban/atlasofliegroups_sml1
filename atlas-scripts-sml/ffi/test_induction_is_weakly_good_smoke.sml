use "atlas-scripts-sml/induction.sml";

fun assertTrue msg b = if b then () else raise Fail ("assertTrue: " ^ msg);

val G = AtlasFFI.atlas_group_new_simple (#"G", 2, #"s", 0);
val L = Induction.Levi G ([0], 0);

val pL = AtlasFFI.atlas_param_trivial L;
val () = assertTrue "expected non-null pL" (pL <> Foreign.Memory.null);

val ok = Induction.is_weakly_good (pL, G, L);
val () = assertTrue "expected trivial on Levi weakly good" ok;

val () = AtlasFFI.atlas_param_free pL;
val () = AtlasFFI.atlas_group_free L;
val () = AtlasFFI.atlas_group_free G;

val () = print "OK\n";

