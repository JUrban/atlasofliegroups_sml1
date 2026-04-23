use "atlas-scripts-sml/combinatorics.sml";

(*
  File: atlas-scripts-sml/ffi/test_combinatorics_more_smoke.sml

  Purpose
  - Smoke test for additional combinatorics helpers in `combinatorics.sml`.
*)

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val () = assert "multinom(2,1,0)=3" (Combinatorics.multinomIntInf [2, 1, 0] = 3);
val () = assert "multi_choose(3,2)=6" (Combinatorics.multi_chooseIntInf (3, 2) = 6);
val () = assert "falling_power(5,3)=60" (Combinatorics.falling_powerIntInf (5, 3) = 60);
val () = assert "rising_power(5,3)=210" (Combinatorics.rising_powerIntInf (5, 3) = 210);

val () = assert "even_places" (Combinatorics.even_places [0, 1, 2, 3, 4] = [0, 2, 4]);
val () = assert "odd_places" (Combinatorics.odd_places [0, 1, 2, 3, 4] = [1, 3]);

val pi = [2, 0, 1]; (* 0->2, 1->0, 2->1 *)
val () = assert "permute_vec" (Combinatorics.permute_vec (pi, [10, 11, 12]) = [11, 12, 10]);
val () = assert "permute_list" (Combinatorics.permute_list (pi, [#"a", #"b", #"c"]) = [#"b", #"c", #"a"]);

val p0 = [1, 2, 0];
val p1 = [2, 0, 1];
val prod = Combinatorics.permutation_product [p0, p1]; (* p0∘p1 *)
val () = assert "permutation_product" (prod = [0, 1, 2]); (* p0(p1(x)) = id for these choices *)

