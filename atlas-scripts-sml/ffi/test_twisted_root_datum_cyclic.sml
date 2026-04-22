use "atlas-scripts-sml/twisted_root_datum.sml";
use "atlas-scripts-sml/MatrixAT.sml";
use "atlas-scripts-sml/IntMatrix.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val rd = RootDatum.newSimple (#"A", 1, false);
val tau = MatrixAT.id_mat 1;
val {rd = rd2, delta} = TwistedRootDatum.cyclic_twist (rd, tau, 3);

val _ = assert "rank multiplies" (RootDatum.rank rd2 = 3);
val _ = assert "delta is 3x3" (IntMatrix.matShape delta = (3, 3));
val y = IntMatrix.matVecMul (delta, [10, 20, 30]);
val _ = assert "cycle action (x1,x2,x3)->(x2,x3,x1)" (y = [20, 30, 10]);

val () = RootDatum.free rd;
val () = RootDatum.free rd2;

val _ = print "ok\n";
