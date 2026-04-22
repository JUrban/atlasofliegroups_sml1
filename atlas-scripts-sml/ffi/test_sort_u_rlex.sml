use "atlas-scripts-sml/sort.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val xs = [[2, 0], [1, 5], [2, 0], [1, 4]];
val ys = Sort.sort_u_rlex xs;
val _ = assert "sorted unique" (ys = [[1, 4], [1, 5], [2, 0]]);

val _ = print "ok\n";

