use "atlas-scripts-sml/twisted_root_datum.sml";
use "atlas-scripts-sml/MatrixAT.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val rd = RootDatum.newSimple (#"A", 1, false);
val trd = {rd = rd, delta = MatrixAT.id_mat 1};
val (frd, tMat) = TwistedRootDatum.folded trd;
val a = TwistedRootDatum.affine_root_of_factor (trd, frd, tMat);
val _ = assert "affine root is highest root in order 1 case" (a = [2]);

val () = RootDatum.free rd;
val () = RootDatum.free frd;

val _ = print "ok\n";

