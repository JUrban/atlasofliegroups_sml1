use "atlas-scripts-sml/RootDatum.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val rd = RootDatum.newSimple (#"F", 4, false);
val roots = RootDatum.rootsCols rd;
val coroots = RootDatum.corootsCols rd;

val _ = assert "roots/coroots same length" (length roots = length coroots);
val _ = assert "F4 has 48 roots" (length roots = 48);
val _ = assert "roots dim 4" (List.all (fn v => length v = 4) roots);
val _ = assert "coroots dim 4" (List.all (fn v => length v = 4) coroots);

val s0 = List.nth (RootDatum.simpleRootsCols rd, 0);
val _ = assert "corootOfRoot finds something" (Option.isSome (RootDatum.corootOfRoot rd s0));

val () = RootDatum.free rd;
val _ = print "ok\n";

