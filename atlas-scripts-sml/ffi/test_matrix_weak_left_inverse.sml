use "atlas-scripts-sml/IntMatrix.sml";
use "atlas-scripts-sml/MatrixAT.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

fun idMat n =
  List.tabulate (n, fn i => List.tabulate (n, fn j => if i = j then 1 else 0));

fun scale (k: int, a: IntMatrix.mat) : IntMatrix.mat =
  List.map (fn row => List.map (fn x => k * x) row) a;

val a = [[2], [0], [0]]; (* 3x1 injective *)
val (j, d) = MatrixAT.weak_left_inverse a;
val ja = IntMatrix.matMul (j, a);
val _ = assert "J*A = d*I" (ja = scale (d, idMat 1));
val _ = assert "d=2" (d = 2);

val a2 = [[1, 0], [0, 1], [0, 0]]; (* saturated embedding *)
val j2 = MatrixAT.left_inverse a2;
val _ = assert "left_inverse works" (IntMatrix.matMul (j2, a2) = idMat 2);

val _ = print "ok\n";

