use "atlas-scripts-sml/LatticeAT.sml";
use "atlas-scripts-sml/IntMatrix.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val a = [[0, 1], [1, 0]];
val m = [[1, 0], [0, 1]];
val b = LatticeAT.restrict_action (a, m);
val _ = assert "restrict_action(I) gives A" (b = a);

val b2 = LatticeAT.corestrict_action (m, a);
val _ = assert "corestrict_action(I) gives A" (b2 = a);

val m3 = [[1, 0]]; (* 1x2 *)
val a3 = [[2, 0], [0, 3]];
val b3 = LatticeAT.corestrict_action (m3, a3);
val _ = assert "corestrict_action picks 2" (b3 = [[2]]);

val _ = print "ok\n";

