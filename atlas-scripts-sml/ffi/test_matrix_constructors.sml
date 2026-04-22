use "atlas-scripts-sml/MatrixAT.sml";
use "atlas-scripts-sml/IntMatrix.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val z = MatrixAT.null (2, 3);
val _ = assert "null dims" (z = [[0, 0, 0], [0, 0, 0]]);

val i3 = MatrixAT.id_mat 3;
val _ = assert "id_mat" (i3 = [[1, 0, 0], [0, 1, 0], [0, 0, 1]]);

val a = MatrixAT.id_mat 2;
val b = [[5]];
val blk = MatrixAT.block_matrix (a, b);
val _ = assert "block_matrix" (blk = [[1, 0, 0], [0, 1, 0], [0, 0, 5]]);

val p = MatrixAT.permutation_matrix [1, 2, 0];
val y = IntMatrix.matVecMul (p, [10, 20, 30]);
val _ = assert "perm matrix acts by y[pi[j]]=x[j]" (y = [30, 10, 20]);

val _ = print "ok\n";

