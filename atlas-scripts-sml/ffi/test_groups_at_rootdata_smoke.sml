use "atlas-scripts-sml/groups_at.sml";
use "atlas-scripts-sml/lietypes.sml";

(*
  Smoke test for `atlas-scripts-sml/groups_at.sml`.
*)

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val gl3 = GroupsAT.GL_roots 3;
val _ = assert "GL_roots(3) shape" (IntMatrix.matShape gl3 = (3, 2));
val _ = assert "GL_roots(3) columns are e1-e2, e2-e3"
               (gl3 = [[1, 0], [~1, 1], [0, ~1]]);

val rdSL3 = GroupsAT.SL 3;
val _ = assert "SL(3) is A2" (LieTypes.simple_type (RootDatum.lieType rdSL3) = (#"A", 2));
val _ = assert "SL(3) Cartan is A2"
               (RootDatum.cartanMatrix rdSL3 = [[2, ~1], [~1, 2]]);

val rdSO5 = GroupsAT.SO 5;
val _ = assert "SO(5) is B2" (LieTypes.simple_type (RootDatum.lieType rdSO5) = (#"B", 2));

val () = RootDatum.free rdSL3;
val () = RootDatum.free rdSO5;

val _ = print "ok\n";

