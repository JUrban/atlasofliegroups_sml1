use "atlas-scripts-sml/ratmat.sml";

(*
  File: atlas-scripts-sml/ffi/test_ratmat_inverse_smoke.sml

  Purpose
  - Smoke test for `RatMat.rational_inverse` on a small integral matrix.
*)

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val m : IntMatrix.mat = [[1, 2], [3, 4]];
val inv = RatMat.rational_inverse m;

val prod = RatMat.mul (RatMat.ofIntMat m, inv);
val () = assert "m * inv = I" (RatMat.equal (prod, RatMat.id_mat 2));

