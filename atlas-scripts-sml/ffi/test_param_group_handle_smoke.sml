use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/ffi/test_param_group_handle_smoke.sml

  Purpose
  - Smoke test for `AtlasFFI.atlas_param_group_handle` (borrowed group pointer).
*)

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val g = AtlasFFI.atlas_group_new_simple (#"A", 1, #"s", 0);
val () = assert "group new" (g <> Foreign.Memory.null);

val p = AtlasFFI.atlas_param_trivial g;
val () = assert "param trivial" (p <> Foreign.Memory.null);

val g2 = AtlasFFI.atlas_param_group_handle p;
val () = assert "param group handle" (g2 <> Foreign.Memory.null);

val r1 = AtlasFFI.atlas_group_rank g;
val r2 = AtlasFFI.atlas_group_rank g2;
val () = assert "borrowed group rank matches" (r1 = r2 andalso r1 = 1);

val () = AtlasFFI.atlas_param_free p;
val () = AtlasFFI.atlas_group_free g;
