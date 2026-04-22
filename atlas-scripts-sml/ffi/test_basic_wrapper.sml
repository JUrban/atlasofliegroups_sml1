use "atlas-scripts-sml/basic.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val m = Basic.matrix ((2, 3), fn (i, j) => 10 * i + j);
val _ = assert "matrix constructor" (m = [[0, 1, 2], [10, 11, 12]]);

val v = Basic.vector (4, fn i => i * i);
val _ = assert "vector constructor" (v = [0, 1, 4, 9]);

val _ = assert "id_mat" (Basic.id_mat 2 = [[1, 0], [0, 1]]);

val _ = print "ok\n";

