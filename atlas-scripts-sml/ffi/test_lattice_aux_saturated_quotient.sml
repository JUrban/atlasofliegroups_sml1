use "atlas-scripts-sml/LatticeAT.sml";
use "atlas-scripts-sml/IntMatrix.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val z2 = [[1, 0], [0, 1]];
val e1 = [[1], [0]];
val twoe1 = [[2], [0]];

val _ = assert "e1 sublattice of Z2" (LatticeAT.is_sublattice (e1, z2));
val _ = assert "twoe1 sublattice of e1" (LatticeAT.is_sublattice (twoe1, e1));
val _ = assert "twoe1 not equal e1" (not (LatticeAT.is_lattice_equal (twoe1, e1)));

val _ = assert "e1 saturated in Z2" (LatticeAT.is_saturated (z2, e1));
val _ = assert "twoe1 not saturated in Z2" (not (LatticeAT.is_saturated (z2, twoe1)));

val q = LatticeAT.quotient (twoe1, e1);
val _ = assert "quotient e1/twoe1 is Z/2" (q = [2]);

val _ = print "ok\n";

