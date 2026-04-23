use "atlas-scripts-sml/polynomial.sml";

(*
  File: atlas-scripts-sml/ffi/test_polynomial_smoke.sml

  Purpose
  - Smoke test for `atlas-scripts-sml/polynomial.sml`.
*)

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

open Polynomial;

val p = [1, 2, 3]; (* 1 + 2X + 3X^2 *)
val () = assert "strip keeps" (strip p = p);
val () = assert "eval_int" (eval_int (p, 2) = 17);
val () = assert "evaluate_at_1" (evaluate_at_1 p = 6);

val a : i_poly_mat =
  [ [poly_1, [1, 1]]
  , [poly_0, poly_1]
  ];
val b : i_poly_mat =
  [ [poly_1, [0, 1]]
  , [poly_0, poly_1]
  ];

val () = assert "matMul dims" (matMul (a, b) = [[poly_1, [1, 2]], [poly_0, poly_1]]);
val () = assert "matAdd" (matAdd (a, b) = [[poly_2, [1, 2]], [poly_0, poly_2]]);

val u : i_poly_mat =
  [ [poly_1, [1, 1], [2]]
  , [poly_0, poly_1, poly_q]
  , [poly_0, poly_0, poly_1]
  ];
val uinv = upper_unitriangular_inverse u;
val () = assert "upper_unitriangular_inverse left" (matMul (u, uinv) = identity_poly_matrix 3);
val () = assert "upper_unitriangular_inverse right" (matMul (uinv, u) = identity_poly_matrix 3);
