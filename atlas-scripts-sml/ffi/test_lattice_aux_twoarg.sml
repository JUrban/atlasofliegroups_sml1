use "atlas-scripts-sml/LatticeAT.sml";
use "atlas-scripts-sml/IntMatrix.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val z2 = [[1, 0], [0, 1]];
val e1 = [[1], [0]];
val e2 = [[0], [1]];
val twoe1 = [[2], [0]];

val q = LatticeAT.free_quotient_lattice_basis_LM (z2, e1);
val _ = assert "free quotient basis of Z2/e1 is e2" (q = e2);

val sq = LatticeAT.saturation_quotient_basis_ML (z2, e1);
val _ = assert "saturation quotient basis of Z2 mod e1 is e2" (sq = e2);

val sq2 = LatticeAT.saturation_quotient_basis_ML (z2, twoe1);
val _ = assert "saturation quotient basis of Z2 mod 2e1 is e2" (sq2 = e2);

val sq3 = LatticeAT.saturation_quotient_basis_ML (e2, e1);
val _ = assert "saturation quotient basis of e2 mod e1 is e2" (sq3 = e2);

val _ = print "ok\n";

