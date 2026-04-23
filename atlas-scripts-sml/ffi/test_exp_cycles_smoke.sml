use "atlas-scripts-sml/exp-generating-series.sml";

(*
  File: atlas-scripts-sml/ffi/test_exp_cycles_smoke.sml

  Purpose
  - Smoke test for `count_permutations_with_cycles` in
    `atlas-scripts-sml/exp-generating-series.sml`.
*)

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

structure S = Lazy_lists;
structure E = Exp_generating_series;

val idOnly = E.count_permutations_with_cycles [1];
val () = assert "only 1-cycles => exp(X)" (S.initial 6 idOnly = [1, 1, 1, 1, 1, 1]);

val transpositionsOnly = E.count_permutations_with_cycles [2];
(* exp(X^2/2): coefficients are 0 for odd n;  n!/(2^{n/2}(n/2)!) for even n.
   For n=0,2,4,6 this is 1,1,3,15. *)
val () = assert "only 2-cycles" (S.initial 7 transpositionsOnly = [1, 0, 1, 0, 3, 0, 15]);

