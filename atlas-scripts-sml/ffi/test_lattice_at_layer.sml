use "atlas-scripts-sml/LatticeAT.sml";

val a = [[1, 0], [0, 0]];
val k = LatticeAT.kernel a;
val () = if IntMatrix.matShape k = (2, 1) then () else raise Fail "test_lattice_at_layer: kernel shape";

val id2 = [[1, 0], [0, 1]];
val e = LatticeAT.eigen_lattice (id2, ~1);
val () = if IntMatrix.matShape e = (2, 0) then () else raise Fail "test_lattice_at_layer: eigen_lattice shape";

val () = print "ok\n";

