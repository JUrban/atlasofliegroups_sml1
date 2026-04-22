use "atlas-scripts-sml/affine.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val rd = RootDatum.newSimple (#"A", 1, false);
val _ = assert "highest_short_root(A1)=[2]" (Affine.highest_short_root rd = [2]);

val ad = Affine.affine_datum rd;
val _ = assert "affine_root is highestRoot" (#affine_roots ad = [[2]]);
val _ = assert "affine_coroot is [1]" (#affine_coroots ad = [[1]]);

val () = RootDatum.free rd;
val _ = print "ok\n";

