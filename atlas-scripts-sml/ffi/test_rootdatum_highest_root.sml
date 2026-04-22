use "atlas-scripts-sml/RootDatum.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val rdA1 = RootDatum.newSimple (#"A", 1, false);
val _ = assert "highest_root(A1)=[2]" (RootDatum.highestRoot rdA1 = [2]);
val _ = assert "rootExpression([2])=[1]" (RootDatum.rootExpression rdA1 [2] = [1]);
val () = RootDatum.free rdA1;

val _ = print "ok\n";

