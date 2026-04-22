use "atlas-scripts-sml/LatticeAT.sml";
use "atlas-scripts-sml/IntMatrix.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

fun col (m: IntMatrix.mat, j: int) : int list =
  List.map (fn row => List.nth (row, j)) m;

fun dot (xs: int list, ys: int list) : int =
  List.foldl (op +) 0 (ListPair.mapEq (op *) (xs, ys));

fun matVecMul (a: IntMatrix.mat, x: int list) : int list =
  List.map (fn row => dot (row, x)) a;

val a = [[1, 0], [0, 1]]; (* Z^2 *)
val b = [[1], [0]]; (* span of e1 *)
val (x, y) = LatticeAT.intersection_plus (a, b);
val (nx, mx) = IntMatrix.matShape x;
val _ = assert "intersection rows" (nx = 2);
val _ = assert "intersection nonempty" (mx >= 1);
val v = col (x, 0);
val _ = assert "v in span(b)" (matVecMul (b, [List.nth (v, 0)]) = v);

val a2 = [[2], [0]];
val b2 = [[0], [3]];
val x2 = LatticeAT.intersection (a2, b2);
val (_, mx2) = IntMatrix.matShape x2;
val _ = assert "disjoint intersection is 0" (mx2 = 0);

val _ = print "ok\n";

