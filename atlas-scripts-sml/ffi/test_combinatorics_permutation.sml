use "atlas-scripts-sml/combinatorics.sml";
use "atlas-scripts-sml/IntMatrix.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val pi = Combinatorics.cyclic_permutation 4 [1, 3, 2]; (* 1->3->2->1, 0 fixed *)
val _ = assert "pi is permutation" (Combinatorics.is_permutation pi);
val inv = Combinatorics.inverse pi;
val _ = assert "inv is permutation" (Combinatorics.is_permutation inv);

val id = Combinatorics.compose_permutations (inv, pi);
val _ = assert "inv o pi = id" (id = [0, 1, 2, 3]);

val pmat = Combinatorics.permutation_matrix pi;
val y = IntMatrix.matVecMul (pmat, [10, 20, 30, 40]);
(* y[pi[j]] = x[j]; pi=[0,3,1,2] so y = [10,30,40,20] *)
val _ = assert "permutation_matrix action" (y = [10, 30, 40, 20]);

val _ = print "ok\n";

