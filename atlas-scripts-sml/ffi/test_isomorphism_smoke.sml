use "atlas-scripts-sml/isomorphism.sml";

(*
  Smoke test for `atlas-scripts-sml/isomorphism.sml`.
*)

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val rdA2 = RootDatum.newSimple (#"A", 2, false);
val rdB2 = RootDatum.newSimple (#"B", 2, false);

val permsA2 = Isomorphism.root_permutations (rdA2, rdA2);
val _ = assert "A2 has 2 diagram automorphisms" (length permsA2 = 2);

val (okP, p) = Isomorphism.root_permutation (rdA2, rdA2);
val _ = assert "root_permutation(A2,A2) succeeds" okP;
val _ = assert "root_permutation witness is 2x2" (IntMatrix.matShape p = (2, 2));

val (okIso, g, _) = Isomorphism.isomorphism_long (rdA2, rdA2);
val _ = assert "isomorphism_long(A2,A2) succeeds" okIso;
val _ = assert "identity is an isomorphism for A2" (g = IntMatrix.identity 2);

val permsA2B2 = Isomorphism.root_permutations (rdA2, rdB2);
val _ = assert "A2 and B2 are not locally isomorphic" (null permsA2B2);

val _ = RootDatum.free rdA2;
val _ = RootDatum.free rdB2;

val _ = print "ok\n";

