use "atlas-scripts-sml/LatticeAT.sml";
use "atlas-scripts-sml/IntMatrix.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

fun zeros (n: int, m: int) : IntMatrix.mat =
  List.tabulate (n, fn _ => List.tabulate (m, fn _ => 0));

val a = [[2, 4, 6], [0, 2, 4]]; (* 2x3 *)
val (m, c) = LatticeAT.image_lattice_plus a;
val ac = IntMatrix.matMul (a, c);
val _ = assert "A*C = M" (ac = m);

val (mr, mc) = IntMatrix.matShape m;
val _ = assert "M has <= cols" (mc <= 3 andalso mr = 2);

val a0 = [[], []] : IntMatrix.mat;
val (m0, c0) = LatticeAT.image_lattice_plus a0;
val _ = assert "empty stays empty" (m0 = [[], []] andalso c0 = []);

val _ = print "ok\n";

