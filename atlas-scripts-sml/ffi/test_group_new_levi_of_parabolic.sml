use "atlas-scripts-sml/ffi/AtlasFFI.sml";

fun assertTrue msg b = if b then () else raise Fail ("assertTrue: " ^ msg);

val g = AtlasFFI.atlas_group_new_simple (#"G", 2, #"s", 0);

val L = AtlasFFI.atlas_group_new_levi_of_parabolic (g, "0", 0);
val () = assertTrue "expected non-null Levi handle" (L <> Foreign.Memory.null);

val rG = AtlasFFI.atlas_group_rank g;
val rL = AtlasFFI.atlas_group_rank L;
val ssL = AtlasFFI.atlas_group_semisimple_rank L;
val () = assertTrue "expected Levi ambient rank matches G" (rL = rG);
val () = assertTrue "expected Levi semisimple rank=1 for S=[0]" (ssL = 1);

val () = assertTrue "expected Levi has nonempty KGB" (AtlasFFI.atlas_group_kgb_size L > 0);

val () = AtlasFFI.atlas_group_free L;
val () = AtlasFFI.atlas_group_free g;
val () = print "OK\n";

