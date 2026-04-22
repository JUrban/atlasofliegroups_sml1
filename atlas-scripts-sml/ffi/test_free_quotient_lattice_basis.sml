use "atlas-scripts-sml/LatticeAT.sml";
use "atlas-scripts-sml/IntMatrix.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val e1 = [[1], [0]];
val q = LatticeAT.free_quotient_lattice_basis e1;
val _ = assert "quotient basis is e2" (q = [[0], [1]]);

val twoe1 = [[2], [0]];
val _ =
  (ignore (LatticeAT.free_quotient_lattice_basis twoe1); raise Fail "expected exception")
  handle Fail _ => ();

val q2 = LatticeAT.saturation_quotient_basis twoe1;
val _ = assert "saturation quotient basis is e2" (q2 = [[0], [1]]);

val _ = print "ok\n";

