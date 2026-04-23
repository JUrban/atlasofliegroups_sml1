use "atlas-scripts-sml/exp-generating-series.sml";

(*
  File: atlas-scripts-sml/ffi/test_exp_generating_series_smoke.sml

  Purpose
  - Smoke test for `atlas-scripts-sml/exp-generating-series.sml`.
*)

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

structure S = Lazy_lists;
structure E = Exp_generating_series;

val eX : S.inf_list = S.inf_ones; (* coefficients of exp(X) are all 1 *)
val e2X = E.exp_multiply (eX, eX); (* exp(2X) => coeffs 2^n *)

val first6 = S.initial 6 e2X;
val () = assert "exp(2X) coeffs" (first6 = [1, 2, 4, 8, 16, 32]);

val quot = E.exp_divide (e2X, eX); (* exp(2X)/exp(X) = exp(X) *)
val first6q = S.initial 6 quot;
val () = assert "division" (first6q = [1, 1, 1, 1, 1, 1]);

val diff = E.exp_diff eX;
val () = assert "diff exp = exp" (S.initial 5 diff = [1, 1, 1, 1, 1]);

