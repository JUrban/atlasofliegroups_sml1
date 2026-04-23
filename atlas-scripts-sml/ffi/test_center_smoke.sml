use "atlas-scripts-sml/groups_at.sml";
use "atlas-scripts-sml/center.sml";

(*
  Smoke test for `atlas-scripts-sml/center.sml` (RootDatum-focused port).
*)

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val rdSL2 = GroupsAT.SL 2;
val rdSL3 = GroupsAT.SL 3;

val _ = assert "SL2 center is Z/2" (Center.type_center rdSL2 = [2]);
val _ = assert "SL3 center is Z/3" (Center.type_center rdSL3 = [3]);

val _ = assert "order_center(SL3)=3" (Center.order_center rdSL3 = 3);
val _ = assert "SL3 center cyclic" (Center.has_cyclic_center rdSL3);

val ((gens, orders), rad) = Center.Z_hat rdSL3;
val _ = assert "Z_hat(SL3) orders=[3]" (orders = [3]);
val _ = assert "Z_hat(SL3) has no radical" (#2 (IntMatrix.matShape rad) = 0);
val _ = assert "Z_hat(SL3) torsion generators has 1 column" (#2 (IntMatrix.matShape gens) = 1);

val () = RootDatum.free rdSL2;
val () = RootDatum.free rdSL3;

val _ = print "ok\n";

