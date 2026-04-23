use "atlas-scripts-sml/is_normal.sml";
use "atlas-scripts-sml/groups.sml";

(*
  Smoke test for `atlas-scripts-sml/is_normal.sml`.
*)

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val g = Groups.quasisplit (#"A", 1, #"s"); (* SL(2,R) *)
val p = AtlasFFI.atlas_param_trivial g;
val () = assert "trivial param allocated" (p <> Foreign.Memory.null);

val (ok, witness) = IsNormal.is_normal_param p;
val () = assert "trivial param is normal" ok;
val () = assert "witness is -1 on success" (witness = ~1);

val () = AtlasFFI.atlas_param_free p;
val () = AtlasFFI.atlas_group_free g;

val _ = print "ok\n";

