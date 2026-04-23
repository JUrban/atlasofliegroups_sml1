use "atlas-scripts-sml/number_theory.sml";

(*
  File: atlas-scripts-sml/ffi/test_number_theory_smoke.sml

  Purpose
  - Smoke test for `atlas-scripts-sml/number_theory.sml`.
*)

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val first10 = Lazy_lists.initial 10 Number_theory.primes;
val () = assert "first primes" (first10 = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]);

val f60 = Number_theory.factorization 60;
val () = assert "factorization 60" (f60 = [(2, 2), (3, 1), (5, 1)]);

val () = assert "phi 60" (Number_theory.phi 60 = 16);

val inv3_11 = Number_theory.inverse_mod (3, 11);
val () = assert "inverse_mod 3 11" (inv3_11 = 4);

val () = assert "power_mod" (Number_theory.power_mod (3, 5, 11) = 1);
val () = assert "Fermat" (Number_theory.test_Fermat (2, 13));

val () = assert "is_prime 1" (not (Number_theory.is_prime 1));
val () = assert "is_prime 2" (Number_theory.is_prime 2);
val () = assert "is_prime 221" (not (Number_theory.is_prime 221)); (* 13*17 *)

