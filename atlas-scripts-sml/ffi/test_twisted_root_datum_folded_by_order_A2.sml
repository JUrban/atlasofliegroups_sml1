use "atlas-scripts-sml/twisted_root_datum.sml";
use "atlas-scripts-sml/MatrixAT.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val rd = RootDatum.newSimple (#"A", 2, false);
val trd = TwistedRootDatum.folded_by_order (rd, 2);
val _ = assert "order(delta)=2" (MatrixAT.order (#delta trd) = 2);
val _ = assert "distinguished" (TwistedRootDatum.is_distinguished (#rd trd, #delta trd));

val () = RootDatum.free rd;
val _ = print "ok\n";

