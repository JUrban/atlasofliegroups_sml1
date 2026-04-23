use "atlas-scripts-sml/K_norm.sml";

(*
  Smoke test for `atlas-scripts-sml/K_norm.sml`.
*)

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);
val rd = AtlasFFI.atlas_group_rootdatum_new g;
val () = assert "rootdatum allocated" (rd <> Foreign.Memory.null);

val p = AtlasFFI.atlas_param_trivial g;
val () = assert "param_trivial ok" (p <> Foreign.Memory.null);

val t = KType.ofParam p;
val h0 = KType.height t;
val h1 = K_norm.K_norm_param rd p;
val () = assert "K_norm_param matches KType.height" (h0 = h1);

val pol = KType.K_type_formula (t, 6);
val () = assert "K_type_formula ok" (pol <> Foreign.Memory.null);

val hs = K_norm.K_norms_ktypepol rd pol;
val hpol = K_norm.K_norm_ktypepol rd pol;
val () = assert "K_norm_ktypepol is max of norms" (hpol = List.foldl Int.max 0 hs);

val _ = KTypePol.free pol;
val _ = KType.free t;
val _ = AtlasFFI.atlas_param_free p;
val _ = RootDatum.free rd;
val _ = AtlasFFI.atlas_group_free g;

val _ = print "test_K_norm_smoke: ok\n";

