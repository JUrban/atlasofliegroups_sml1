use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/KTypePol.sml";
use "atlas-scripts-sml/c_form_branch.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val g = AtlasFFI.atlas_group_new_simple (#"A", 1, #"s", 0);
val p = AtlasFFI.atlas_param_trivial g;

val cf = C_form_branch.c_form_branch_irr (p, 6);
val rank = AtlasFFI.atlas_group_rank g;
val () = assert "c_form_branch produced pure-ish polynomial handle" (cf <> Foreign.Memory.null);
val () = ignore (KTypePol.purityCounts (cf, rank));
val () = KTypePol.free cf;

val () = AtlasFFI.atlas_param_free p;
val () = AtlasFFI.atlas_group_free g;

val () = print "OK: c_form_branch_smoke\n";

