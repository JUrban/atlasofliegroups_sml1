use "atlas-scripts-sml/combinatorics.sml";

(*
  File: atlas-scripts-sml/ffi/test_combinatorics_binom_smoke.sml

  Purpose
  - Smoke test for the `binom` and combination encode/decode utilities added to
    `atlas-scripts-sml/combinatorics.sml`.
*)

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val () = assert "binom(5,2)" (Combinatorics.binom (5, 2) = 10);
val () = assert "binom(10,0)" (Combinatorics.binom (10, 0) = 1);
val () = assert "binom(10,10)" (Combinatorics.binom (10, 10) = 1);

val cs = [2, 5, 7];
val n = Combinatorics.combination_encode cs;
val cs' = (Combinatorics.combination_decode (length cs)) n;
val () = assert "combination roundtrip" (cs = cs');

