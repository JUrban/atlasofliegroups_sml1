use "atlas-scripts-sml/twisted_root_datum.sml";
use "atlas-scripts-sml/MatrixAT.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val rd = RootDatum.newSimple (#"A", 1, false);
val trd = {rd = rd, delta = MatrixAT.id_mat 1};
val (frd, tMat) = TwistedRootDatum.folded trd;

val _ = assert "folded rank is 1" (RootDatum.rank frd = 1);
val _ = assert "T is 1x1 identity" (tMat = [[1]]);

val _ = assert "folded simpleRoots=[2]" (RootDatum.simpleRootsCols frd = [[2]]);
val _ = assert "folded simpleCoroots=[1]" (RootDatum.simpleCorootsCols frd = [[1]]);

val () = RootDatum.free rd;
val () = RootDatum.free frd;

val _ = print "ok\n";

