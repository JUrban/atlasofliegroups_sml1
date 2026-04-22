use "atlas-scripts-sml/RootDatum.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val rd = RootDatum.fromLieType (LieType.parse "A1A1");
val _ = assert "rank A1A1 is 2" (RootDatum.rank rd = 2);
val _ = assert "ssRank A1A1 is 2" (RootDatum.semisimpleRank rd = 2);
val _ = assert "num factors is 2" (RootDatum.numberSimpleFactors rd = 2);

val () = RootDatum.free rd;
val _ = print "ok\n";

