use "atlas-scripts-sml/basic.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val xs = [1, 3, 3, 5, 8, 13];
val find = Basic.binary_search_in (xs, op <=);
val _ = assert "find 1" (find 1 = SOME 0);
val _ = assert "find 3 (some index)" (Option.isSome (find 3));
val _ = assert "find 4 none" (find 4 = NONE);
val _ = assert "find 13" (find 13 = SOME 5);

val _ = print "ok\n";

