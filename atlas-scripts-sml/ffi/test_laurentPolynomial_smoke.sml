use "atlas-scripts-sml/laurentPolynomial.sml";

(*
  File: atlas-scripts-sml/ffi/test_laurentPolynomial_smoke.sml

  Purpose
  - Smoke test for `atlas-scripts-sml/laurentPolynomial.sml`.
*)

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

open LaurentPolynomial;

val f = {w = [0, 0, 3, 4], n = ~5};
val fnorm = normalize f;
val () = assert "normalize drops leading zeros" (#w fnorm = [3, 4] andalso #n fnorm = ~3);

val g = v_laurent_power 2; (* v^2 *)
val h = mul (f, g);
val () = assert "mul shifts power" (lowest_power h = lowest_power f + 2);

val s = add (v_laurent, v_inverse);
val () = assert "v_sum matches" (equal (s, v_sum));

val p = laurent_poly_as_poly (poly_as_laurent_poly [1, 2, 3]);
val () = assert "poly roundtrip" (p = [1, 2, 3]);

