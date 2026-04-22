use "atlas-scripts-sml/RootDatum.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val rdA1 = RootDatum.newSimple (#"A", 1, false);
val d = RootDatum.dual rdA1;
val _ = assert "dual rank" (RootDatum.rank d = 1);
val _ = assert "highest_short_root(A1)=[2]" (RootDatum.highestShortRoot rdA1 = [2]);
val () = RootDatum.free d;
val () = RootDatum.free rdA1;

val _ = print "ok\n";

