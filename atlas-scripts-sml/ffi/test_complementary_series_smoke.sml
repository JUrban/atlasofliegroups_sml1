use "atlas-scripts-sml/complementary_series.sml";
use "atlas-scripts-sml/groups.sml";

(*
  Smoke test for `atlas-scripts-sml/complementary_series.sml`.
*)

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val g = Groups.quasisplit (#"A", 1, #"s"); (* SL(2,R) *)
val p = AtlasFFI.atlas_param_trivial g;
val () = assert "trivial param allocated" (p <> Foreign.Memory.null);

val (deformable, q) = ComplementarySeries.end_of_complementary_series p;
val () = assert "returned param allocated" (q <> Foreign.Memory.null);

val () = AtlasFFI.atlas_param_free q;
val () = AtlasFFI.atlas_param_free p;
val () = AtlasFFI.atlas_group_free g;

val _ = print ("ok (" ^ Bool.toString deformable ^ ")\n");

