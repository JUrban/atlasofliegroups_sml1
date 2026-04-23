use "atlas-scripts-sml/parameters.sml";
use "atlas-scripts-sml/groups.sml";

(*
  Smoke test for `atlas-scripts-sml/parameters.sml`.
*)

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val g = Groups.quasisplit (#"A", 1, #"s"); (* SL(2,R) *)
val () = assert "square(g) has correct length"
               (length (#nums (Parameters.square g)) = AtlasFFI.atlas_group_rank g);

val p0 = AtlasFFI.atlas_param_trivial g;
val () = assert "trivial param allocated" (p0 <> Foreign.Memory.null);

val p1 = Parameters.contragredient p0;
val () = assert "contragredient allocated" (p1 <> Foreign.Memory.null);
val () = assert "contragredient(trivial)=trivial"
               (AtlasFFI.atlas_param_equal (p0, p1) = 1);

val () = AtlasFFI.atlas_param_free p1;
val () = AtlasFFI.atlas_param_free p0;
val () = AtlasFFI.atlas_group_free g;

val _ = print "ok\n";

