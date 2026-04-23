use "atlas-scripts-sml/inverse.sml";

(*
  File: atlas-scripts-sml/ffi/test_inverse_smoke.sml

  Purpose
  - Smoke test for `atlas-scripts-sml/inverse.sml` + `polynomial.sml`.
  - Builds a small 3×3 upper unitriangular polynomial matrix and checks that
    `Inverse.inverse` and `Inverse.unitri_inv` produce a two-sided inverse.
*)

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

open Polynomial;

val m : i_poly_mat =
  [ [poly_1, [1, 1], [0, 1]]        (* 1, 1+q, q *)
  , [poly_0, poly_1, [2]]           (* 0, 1, 2 *)
  , [poly_0, poly_0, poly_1]        (* 0, 0, 1 *)
  ];

val inv1 = Inverse.inverse m;
val inv2 = Inverse.unitri_inv m;

val id3 = identity_poly_matrix 3;

val () = assert "inverse: inv*m = I" (matMul (inv1, m) = id3);
val () = assert "inverse: m*inv = I" (matMul (m, inv1) = id3);
val () = assert "unitri_inv: inv*m = I" (matMul (inv2, m) = id3);
val () = assert "unitri_inv: m*inv = I" (matMul (m, inv2) = id3);

