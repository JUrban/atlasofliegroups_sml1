use "atlas-scripts-sml/twisted_root_datum.sml";
use "atlas-scripts-sml/MatrixAT.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val rd = RootDatum.newSimple (#"A", 1, false);
val trd = {rd = rd, delta = MatrixAT.id_mat 1};
val (roots, coroots) = TwistedRootDatum.pre_folded trd;

val _ = assert "roots = [[2]]" (roots = [[2]]);
val _ = assert "coroots = [[1]]" (coroots = [[1]]);

val () = RootDatum.free rd;
val _ = print "ok\n";

