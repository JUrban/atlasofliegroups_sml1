use "atlas-scripts-sml/matrix.sml";
use "atlas-scripts-sml/IntMatrix.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val a = [[1, 0]];
val b = Matrix.right_inverse a;
val _ = assert "A*right_inverse(A)=I" (IntMatrix.matMul (a, b) = [[1]]);

val _ = print "ok\n";

