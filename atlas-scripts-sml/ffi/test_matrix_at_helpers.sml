use "atlas-scripts-sml/MatrixAT.sml";
use "atlas-scripts-sml/IntMatrix.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val m = [[2, 4], [6, 8]];
val (d, m1) = MatrixAT.factor_scalar m;
val _ = assert "factor_scalar gcd=2" (d = 2);
val _ = assert "factor_scalar divides entries" (m1 = [[1, 2], [3, 4]]);

val m3 = [[1, 2, 3], [4, 5, 6], [7, 8, 9]];
val p = MatrixAT.principal_submatrix (m3, [0, 2]);
val _ = assert "principal_submatrix" (p = [[1, 3], [7, 9]]);

val tl = MatrixAT.top_left_square_block (m3, 2);
val _ = assert "top_left_square_block" (tl = [[1, 2], [4, 5]]);

val a = [[1, 0], [0, 1]];
val _ = assert "row_echelon(I)=I" (MatrixAT.row_echelon a = a);

val e1 = [[1], [0]];
val merged = MatrixAT.merge_matrices [a, e1];
val _ = assert "merge_matrices shapes" (merged = [[1, 0, 1], [0, 1, 0]]);

val _ = print "ok\n";

