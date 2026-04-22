use "atlas-scripts-sml/basic.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val ps = Basic.power_set [1, 2, 3];
val _ = assert "power_set size" (length ps = 8);

val ch = Basic.choices_from ([#"a", #"b", #"c", #"d"], 2);
val _ = assert "choices_from count" (length ch = 6);
val _ = assert "choices_from first" (hd ch = [#"a", #"b"]);

val v = Basic.all_0_1_vecs_with_sum (4, 2);
val _ = assert "0-1 vecs count" (length v = 6);
val _ = assert "each has sum 2" (List.all (fn xs => List.foldl (op +) 0 xs = 2) v);

val _ = print "ok\n";

