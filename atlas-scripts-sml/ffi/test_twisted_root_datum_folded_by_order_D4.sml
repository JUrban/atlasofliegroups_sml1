use "atlas-scripts-sml/twisted_root_datum.sml";
use "atlas-scripts-sml/MatrixAT.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val rd = RootDatum.newSimple (#"D", 4, false);
val trd = TwistedRootDatum.folded_by_order (rd, 3);
val _ = assert "order(delta)=3" (MatrixAT.order (#delta trd) = 3);
val _ = assert "distinguished" (TwistedRootDatum.is_distinguished (#rd trd, #delta trd));

val () = RootDatum.free rd;
val _ = print "ok\n";

