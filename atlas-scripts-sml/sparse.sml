use "atlas-scripts-sml/basic.sml";

(*
  File: atlas-scripts-sml/sparse.sml

  Purpose
  - SML translation of `atlas-scripts/sparse.at`.
  - Implements a tiny sparse-matrix representation (by sparse columns) and
    helpers for multiplying a dense integer matrix by a sparse matrix.

  Representation (matches `.at` semantics)
  - Dense `mat` in Atlas `.at` is a list of *columns* (column-major).
  - We mirror that here:
      type vec = int list
      type mat = vec list     (* columns *)
  - A sparse matrix is given as a list of columns, each a list of pairs
    `(i,coef)` indicating `coef` at row index `i` in that column:
      type sparse_column = (int * int) list
      type sparse_mat    = sparse_column list

  Notes
  - This module does not try to interoperate directly with `IntMatrix.mat`
    (row-major). If you need that, add explicit conversion helpers.
*)

structure Sparse = struct
  type vec = int list
  type mat = vec list (* column-major *)

  type sparse_entry = int * int
  type sparse_column = sparse_entry list
  type sparse_mat = sparse_column list

  fun n_rows (m: mat) : int =
    (case m of
       [] => 0
     | col :: _ => length col)

  fun n_columns (m: mat) : int = length m

  fun assertRect (m: mat) : unit =
    (case m of
       [] => ()
     | col0 :: cols =>
         let
           val n = length col0
         in
           if List.all (fn c => length c = n) cols then () else raise Fail "Sparse: ragged dense mat"
         end)

  (* Dense -> sparse by dropping zero entries (preserves row indices). *)
  fun sparse (m: mat) : sparse_mat =
    let
      val () = assertRect m
      fun colToSparse (c: vec) : sparse_column =
        let
          fun step (coef, (i, acc)) =
            if coef <> 0 then (i + 1, (i, coef) :: acc) else (i + 1, acc)
          val (_, revAcc) = List.foldl step (0, []) c
        in
          List.rev revAcc
        end
    in
      List.map colToSparse m
    end

  (* Sparse -> dense, given the row count. *)
  fun unsparse (s: sparse_mat, height: int) : mat =
    let
      fun buildCol (entries: sparse_column) : vec =
        let
          val v = Array.array (height, 0)
          fun add (i, coef) =
            if i < 0 orelse i >= height then
              raise Fail "Sparse.unsparse: row index out of range"
            else
              Array.update (v, i, Array.sub (v, i) + coef)
          val () = List.app add entries
        in
          Array.foldr (op ::) [] v
        end
    in
      if height < 0 then raise Fail "Sparse.unsparse: negative height" else ();
      List.map buildCol s
    end

  (* Linear combination of columns: sum_i coeffs[i] * cols[i]. *)
  fun linCombCols (cols: mat, coeffs: vec) : vec =
    let
      val () = assertRect cols
      val nCols = n_columns cols
      val nRows = n_rows cols
      val () = if length coeffs = nCols then () else raise Fail "Sparse.linCombCols: dim mismatch"
      val acc = Array.array (nRows, 0)
      fun addScaled (col: vec, k: int) =
        if k = 0 then
          ()
        else
          let
            fun loop ([], r) = ()
              | loop (x :: xs, r) =
                  (Array.update (acc, r, Array.sub (acc, r) + k * x); loop (xs, r + 1))
          in
            loop (col, 0)
          end
      val () = List.app addScaled (ListPair.zipEq (cols, coeffs))
    in
      Array.foldr (op ::) [] acc
    end

  (* Dense matrix multiply (column-major): `a*b`. *)
  fun mulDense (a: mat, b: mat) : mat =
    let
      val () = assertRect a
      val () = assertRect b
      val () = if n_columns a = n_rows b then () else raise Fail "Sparse.mulDense: dim mismatch"
    in
      List.map (fn coeffs => linCombCols (a, coeffs)) b
    end

  (* Dense * sparse (on the right), as in `sparse.at`. *)
  fun mulDenseSparse (m: mat, s: sparse_mat) : mat =
    let
      val () = assertRect m
      val n = n_columns m
      fun oneCol (c: sparse_column) : vec =
        let
          val inds = List.map #1 c
          val coefs = List.map #2 c
          val () = if List.all (fn i => 0 <= i andalso i < n) inds then () else raise Fail "Sparse.mulDenseSparse: index out of range"
          val cols = List.map (fn i => List.nth (m, i)) inds
        in
          linCombCols (cols, coefs)
        end
    in
      List.map oneCol s
    end

  (* Transpose of a dense matrix (column-major result). *)
  fun transposeDense (m: mat) : mat =
    let
      val () = assertRect m
      val (h, w) = (n_rows m, n_columns m)
      fun col i = List.tabulate (w, fn j => List.nth (List.nth (m, j), i))
    in
      List.tabulate (h, col)
    end

  (* Transpose of a sparse matrix; caller provides `height` (#rows). *)
  fun transposeSparse (s: sparse_mat, height: int) : sparse_mat =
    let
      val () = if height >= 0 then () else raise Fail "Sparse.transposeSparse: negative height"
      val acc = Array.array (height, ([]: sparse_entry list))
      fun add (colIdx: int) ((row, entry): sparse_entry) =
        if row < 0 orelse row >= height then
          raise Fail "Sparse.transposeSparse: row index out of range"
        else
          Array.update (acc, row, (colIdx, entry) :: Array.sub (acc, row))
      fun oneCol (colIdx: int, col: sparse_column) : unit =
        List.app (add colIdx) col
      fun loop ([], _) = ()
        | loop (col :: rest, j) = (oneCol (j, col); loop (rest, j + 1))
      val () = loop (s, 0)
      fun revCol xs = List.rev xs
    in
      Array.foldr (fn (c, cols) => revCol c :: cols) [] acc
    end

  (* Sparse * dense via transpose trick from `sparse.at`. *)
  fun mulSparseDense (s: sparse_mat, m: mat) : mat =
    let
      val mt = transposeDense m
      val st = transposeSparse (s, n_columns m)
      val prodT = mulDenseSparse (mt, st)
    in
      transposeDense prodT
    end
end
