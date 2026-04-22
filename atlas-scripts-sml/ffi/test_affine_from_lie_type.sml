use "atlas-scripts-sml/affine.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val ad = Affine.affine_datum_from_lieType (LieType.parse "A1A1");
val _ = assert "affine roots has 1 entry (default highestRoot)" (length (#affine_roots ad) = 1);

val () = RootDatum.free (#rd ad);
val _ = print "ok\n";

