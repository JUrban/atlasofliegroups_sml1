use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  Smoke test for the new LKTs FFI:
  - `atlas_param_LKTs_size`
  - `atlas_param_LKTs_get_ktype_clone`
  - `atlas_param_LKTs_get_mult`
*)

val g = AtlasFFI.atlas_group_new_simple (#"A", 1, #"s", 0);
val p = AtlasFFI.atlas_param_trivial g;

val n = AtlasFFI.atlas_param_LKTs_size p;
val _ = if n <> 1 then raise Fail ("expected 1 LKT for trivial, got " ^ Int.toString n) else ();

val t0 = AtlasFFI.atlas_param_LKTs_get_ktype_clone (p, 0);
val _ = if t0 = Foreign.Memory.null then raise Fail ("LKT handle is null: " ^ AtlasFFI.atlas_last_error ()) else ();
val m0 = AtlasFFI.atlas_param_LKTs_get_mult (p, 0);
val _ = if m0 <> 1 then raise Fail ("expected mult=1, got " ^ Int.toString m0) else ();

val _ = AtlasFFI.atlas_ktype_free t0;
val _ = AtlasFFI.atlas_param_free p;
val _ = AtlasFFI.atlas_group_free g;

val _ = print "OK\n";

