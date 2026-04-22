use "atlas-scripts-sml/MatrixAT.sml";
use "atlas-scripts-sml/IntMatrix.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val i3 = MatrixAT.id_mat 3;
val _ = assert "order(I)=1" (MatrixAT.order i3 = 1);

val p = MatrixAT.permutation_matrix [1, 2, 0];
val _ = assert "3-cycle has order 3" (MatrixAT.order p = 3);

val negI2 = [[~1, 0], [0, ~1]];
val _ = assert "-I has order 2" (MatrixAT.order negI2 = 2);

val _ = assert "matPow works" (MatrixAT.matPow (p, 3) = i3);

val _ = print "ok\n";

