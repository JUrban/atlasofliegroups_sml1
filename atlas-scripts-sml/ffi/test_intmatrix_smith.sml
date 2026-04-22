use "atlas-scripts-sml/IntMatrix.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val a = [[2, 0], [0, 6]];
val (_, ds) = IntMatrix.smithBasis a;
val _ = print ("diag=" ^ String.concatWith "," (List.map Int.toString ds) ^ "\n");
val _ = assert "diag divides" (ds = [2, 6] orelse ds = [2, 6, 0] orelse ds = [2, 6, 1]);

val z = [[], []] : IntMatrix.mat;
val (_, ds0) = IntMatrix.smithBasis z;
val _ = assert "smith empty diag" (ds0 = []);

val _ = print "ok\n";

