use "atlas-scripts-sml/IntMatrix.sml";
use "atlas-scripts-sml/Rat.sml";
use "atlas-scripts-sml/K_highest_weights.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val a = K_highest_weights.cone (Rat.make (3, 1), [Rat.make (1, 1), Rat.make (1, 1)]);
val (n, m) = IntMatrix.matShape a;
val _ = assert "cone dims" (n = 2 andalso m = 10);

val b = K_highest_weights.cone (Rat.make (0, 1), [Rat.make (1, 1), Rat.make (2, 1), Rat.make (5, 1)]);
val (n2, m2) = IntMatrix.matShape b;
val _ = assert "cone(0) gives one column" (n2 = 3 andalso m2 = 1);
val _ = assert "cone(0) is zero weight" (b = [[0], [0], [0]]);

val _ = print "ok\n";

