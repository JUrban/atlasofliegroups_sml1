use "atlas-scripts-sml/twisted_root_datum.sml";
use "atlas-scripts-sml/MatrixAT.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val rd = RootDatum.newSimple (#"A", 1, false);
val trd = {rd = rd, delta = MatrixAT.id_mat 1};
val ad = TwistedRootDatum.affine_datum trd;

val _ = assert "affine_roots=[2]" (#affine_roots ad = [[2]]);
val _ = assert "affine_coroots=[1]" (#affine_coroots ad = [[1]]);

val () = RootDatum.free rd;
val () = RootDatum.free (#rd ad);

val _ = print "ok\n";

