use "atlas-scripts-sml/RootDatum.sml";
use "atlas-scripts-sml/MatrixAT.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val rdA1 = RootDatum.newSimple (#"A", 1, false);
val sr = RootDatum.simpleRootsMat rdA1;
val scr = RootDatum.simpleCorootsMat rdA1;
val sr2 = MatrixAT.block_matrix (sr, sr);
val scr2 = MatrixAT.block_matrix (scr, scr);
val rd = RootDatum.newFromSimpleMats (sr2, scr2, false);

val hrs = RootDatum.highestRoots rd;
val _ = assert "two highest roots" (length hrs = 2);
val _ = assert "each is a root" (List.all (fn r => Option.isSome (RootDatum.corootOfRoot rd r)) hrs);

val hsrs = RootDatum.highestShortRoots rd;
val _ = assert "two highest short roots" (length hsrs = 2);
val _ = assert "each is a root" (List.all (fn r => Option.isSome (RootDatum.corootOfRoot rd r)) hsrs);

val () = RootDatum.free rdA1;
val () = RootDatum.free rd;

val _ = print "ok\n";

