use "atlas-scripts-sml/strong_comp_test.sml";

(* Smoke test: ensure sizes match for small parameters. *)

val g = StrongCompTest.UGG (4, 2);
val (n, e) = StrongCompTest.graph_size g;
val () = if n = 16 then () else raise Fail "strong_comp_test: wrong vertex count";
val () = if e > 0 then () else raise Fail "strong_comp_test: expected some edges";

val line = StrongCompTest.line 5;
val (n2, e2) = StrongCompTest.graph_size line;
val () = if n2 = 5 andalso e2 = 4 then () else raise Fail "strong_comp_test: line size mismatch";

