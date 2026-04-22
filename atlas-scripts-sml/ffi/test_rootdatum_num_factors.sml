use "atlas-scripts-sml/RootDatum.sml";
use "atlas-scripts-sml/MatrixAT.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val rdA1 = RootDatum.newSimple (#"A", 1, false);
val _ = assert "A1 has 1 factor" (RootDatum.numberSimpleFactors rdA1 = 1);

val sr = RootDatum.simpleRootsMat rdA1;     (* [[2]] *)
val scr = RootDatum.simpleCorootsMat rdA1;  (* [[1]] *)
val sr2 = MatrixAT.block_matrix (sr, sr);
val scr2 = MatrixAT.block_matrix (scr, scr);
val rdA1xA1 = RootDatum.newFromSimpleMats (sr2, scr2, false);
val _ = assert "A1xA1 has 2 factors" (RootDatum.numberSimpleFactors rdA1xA1 = 2);

val () = RootDatum.free rdA1;
val () = RootDatum.free rdA1xA1;

val _ = print "ok\n";

