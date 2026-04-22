use "atlas-scripts-sml/poset.sml";

(* Poset: 0 < 1 < 2 (chain). *)
val p : Poset.t = [[1], [2], []];

val c = Poset.closure p;
val () = if c = [[0, 1, 2], [1, 2], [2]] then () else raise Fail "closure mismatch";

val inv = Poset.poset_inverse p;
val () = if inv = [[0], [0, 1], [0, 1, 2]] then () else raise Fail "inverse mismatch";

val () = if Poset.less (p, 0, 2) then () else raise Fail "less should hold";
val () = if not (Poset.less (p, 2, 0)) then () else raise Fail "less should not hold";

val mins = Poset.minimal_nodes p;
val () = if mins = [2] then () else raise Fail "minimal_nodes mismatch";

val basics = Poset.basic_nodes p;
val () = if basics = [2] then () else raise Fail "basic_nodes mismatch";

val () = print "OK\n";

