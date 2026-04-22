use "atlas-scripts-sml/RootDatum.sml";
use "atlas-scripts-sml/MatrixAT.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val rdA1 = RootDatum.newSimple (#"A", 1, false);
val sr = RootDatum.simpleRootsMat rdA1;
val scr = RootDatum.simpleCorootsMat rdA1;
val sr2 = MatrixAT.block_matrix (sr, sr);
val scr2 = MatrixAT.block_matrix (scr, scr);
val rd = RootDatum.newFromSimpleMats (sr2, scr2, false);

val factors = RootDatum.simpleFactorsEmbedded rd;
val _ = assert "two factors" (length factors = 2);
val _ = assert "each factor rank=2" (List.all (fn f => RootDatum.rank f = 2) factors);
val _ = assert "each factor ssRank=1" (List.all (fn f => RootDatum.semisimpleRank f = 1) factors);

val () = List.app RootDatum.free factors;
val () = RootDatum.free rdA1;
val () = RootDatum.free rd;

val _ = print "ok\n";
