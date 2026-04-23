use "atlas-scripts-sml/sparse.sml";

(*
  File: atlas-scripts-sml/ffi/test_sparse_smoke.sml

  Purpose
  - Smoke test for `atlas-scripts-sml/sparse.sml`.
  - Checks `dense * sparse(dense)` agrees with dense multiplication.
*)

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

(* Column-major dense matrices. *)
val a : Sparse.mat = [[1, 0, 2], [~1, 3, 1], [0, 4, ~2]];
val b : Sparse.mat = [[2, 1, 0], [0, ~1, 3], [1, 0, 1]];

val dense = Sparse.mulDense (a, b);
val sb = Sparse.sparse b;
val dense2 = Sparse.mulDenseSparse (a, sb);

val () = assert "dense mul agrees with sparse" (dense = dense2);

val st = Sparse.transposeSparse (sb, Sparse.n_rows b);
val bt = Sparse.transposeDense b;
val () = assert "transposeSparse agrees with transposeDense->sparse"
  (st = Sparse.sparse bt);

