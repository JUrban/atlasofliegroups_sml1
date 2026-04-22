use "atlas-scripts-sml/LatticeAT.sml";
use "atlas-scripts-sml/IntMatrix.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val e1 = [[1], [0]];
val pIm = LatticeAT.projector_to_image e1;
val pComp = LatticeAT.projector_mod_image e1;
val _ = assert "proj image" (pIm = [[1, 0], [0, 0]]);
val _ = assert "proj comp" (pComp = [[0, 0], [0, 1]]);

val (u, v) = LatticeAT.decompose (e1, [7, 5]);
val _ = assert "decompose u" (u = [7, 0]);
val _ = assert "decompose v" (v = [0, 5]);

val a = [[1, 0], [0, 3]];
val q = LatticeAT.quotient_matrix (a, e1);
val _ = assert "quotient action on e2" (q = [[3]]);

val a2 = [[0, 1], [1, 0]];
val q2 = LatticeAT.quotient_matrix (a2, e1);
val _ = assert "swap kills quotient" (q2 = [[0]]);

val _ = print "ok\n";

