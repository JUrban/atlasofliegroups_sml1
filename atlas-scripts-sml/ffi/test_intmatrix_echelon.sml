use "atlas-scripts-sml/IntMatrix.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

fun matEq (a: IntMatrix.mat, b: IntMatrix.mat) = (a = b);

fun zeros (n: int, m: int) : IntMatrix.mat =
  List.tabulate (n, fn _ => List.tabulate (m, fn _ => 0));

val a = [[1, 2, 3], [0, 4, 6]];
val (m, c, piv, eps) = IntMatrix.echelon a;

val (ar, ac) = IntMatrix.matShape a;
val (mr, mc) = IntMatrix.matShape m;
val (cr, cc) = IntMatrix.matShape c;
val _ = assert "dims M rows" (mr = ar);
val _ = assert "C square" (cr = ac andalso cc = ac);
val _ = assert "pivots length" (length piv = mc);
val _ = assert "eps is +/-1" (eps = 1 orelse eps = ~1);

val acm = IntMatrix.matMul (a, c);
val z = zeros (ar, ac - mc);
val expect = IntMatrix.hcat (m, z);
val _ = assert "A*C = [M 0]" (matEq (acm, expect));

val _ = print "ok\n";

