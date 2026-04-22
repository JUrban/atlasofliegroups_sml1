use "atlas-scripts-sml/twisted_root_datum.sml";
use "atlas-scripts-sml/MatrixAT.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val rd = RootDatum.newSimple (#"A", 1, false);
val trd = {rd = rd, delta = MatrixAT.id_mat 1};
val (frd, tMat) = TwistedRootDatum.folded trd;

val inv = TwistedRootDatum.inverse_image_simple_factor (trd, frd, tMat);
val _ = assert "inverse image is A1" (RootDatum.simpleRootsCols inv = [[2]]);

val () = RootDatum.free rd;
val () = RootDatum.free frd;
val () = RootDatum.free inv;

val _ = print "ok\n";

