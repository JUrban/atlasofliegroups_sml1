use "atlas-scripts-sml/twisted_root_datum.sml";
use "atlas-scripts-sml/MatrixAT.sml";
use "atlas-scripts-sml/IntMatrix.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val rdA1 = RootDatum.newSimple (#"A", 1, false);
val rdA2 = RootDatum.newSimple (#"A", 2, false);

val t1 = {rd = rdA1, delta = MatrixAT.id_mat 1};
val t2 = {rd = rdA2, delta = MatrixAT.id_mat 2};
val t3 = TwistedRootDatum.mul (t1, t2);

val _ = assert "rank adds" (RootDatum.rank (#rd t3) = 3);
val _ = assert "delta block diagonal" (#delta t3 = MatrixAT.block_matrix (MatrixAT.id_mat 1, MatrixAT.id_mat 2));

val () = RootDatum.free rdA1;
val () = RootDatum.free rdA2;
val () = RootDatum.free (#rd t3);

val _ = print "ok\n";

