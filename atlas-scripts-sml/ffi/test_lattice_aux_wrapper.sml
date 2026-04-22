use "atlas-scripts-sml/lattice_aux.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val z2 = [[1, 0], [0, 1]];
val e1 = [[1], [0]];
val e2 = [[0], [1]];

val _ = assert "first_columns(1,I2)=e1" (LatticeAux.first_columns (1, z2) = e1);
val _ = assert "first_rows(1,I2)=[[1,0]]" (LatticeAux.first_rows (1, z2) = [[1, 0]]);

val q = LatticeAux.free_quotient_lattice_basis (z2, e1);
val _ = assert "Z2/e1 quotient basis is e2" (q = e2);

val sq = LatticeAux.saturation_quotient_basis (z2, e1);
val _ = assert "sat quotient basis is e2" (sq = e2);

val _ = print "ok\n";

