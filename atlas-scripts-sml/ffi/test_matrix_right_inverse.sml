use "atlas-scripts-sml/MatrixAT.sml";
use "atlas-scripts-sml/IntMatrix.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val a = [[1, 0]]; (* Z^2 -> Z, (x,y) |-> x *)
val b = MatrixAT.right_inverse a;
val _ = assert "A*right_inverse(A)=I" (IntMatrix.matMul (a, b) = [[1]]);

val a2 = [[2, 0]];
val (b2, d2) = MatrixAT.weak_right_inverse a2;
val _ = assert "A2*B2=d2*I" (IntMatrix.matMul (a2, b2) = [[d2]]);
val _ = assert "weak right inverse index is 2" (d2 = 2);

val _ =
  (ignore (MatrixAT.right_inverse a2); raise Fail "expected exception")
  handle Fail _ => ();

val _ = print "ok\n";

