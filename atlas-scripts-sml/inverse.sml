use "atlas-scripts-sml/polynomial.sml";

(*
  File: atlas-scripts-sml/inverse.sml

  Purpose
  - SML translation of `atlas-scripts/inverse.at`.
  - Implements inversion of upper unitriangular polynomial matrices (entries
    are integer polynomials).

  Representation (matching `polynomial.sml`)
  - `Polynomial.i_poly` is `int list` with coefficient of X^k at index k.
  - `Polynomial.i_poly_mat` is a square matrix as a list of rows.

  Implemented functions
  - `inverse`:
      - inverts an upper unitriangular polynomial matrix using column
        operations (Gauss-style), as in `inverse.at`.
  - `unitri_inv`:
      - alternative inversion routine specialized to unitriangular matrices.

  Notes / limitations
  - This is a direct port of the matrix-inversion core. The `regular_KL_matrix`
    helper at the bottom of `inverse.at` depends on KL computations and is not
    included here.
*)

structure Inverse = struct
  open Polynomial

  (* Add c * column i to column j in an n×n matrix (rows representation). *)
  fun column_operation (a: i_poly_mat, i: int, j: int, c: i_poly, n: int) : i_poly_mat =
    let
      fun updateRow row =
        List.tabulate
          ( n
          , fn l =>
              if l = j then
                add (List.nth (row, l), poly_product (c, List.nth (row, i)))
              else
                List.nth (row, l)
          )
    in
      List.tabulate (n, fn k => updateRow (List.nth (a, k)))
    end

  (* Add c * row j to row i in an n×n matrix (rows representation). *)
  fun row_operation (a: i_poly_mat, i: int, j: int, c: i_poly, n: int) : i_poly_mat =
    let
      val source = List.nth (a, j)
      val dest = List.nth (a, i)
      val newDest = List.tabulate (n, fn l => add (List.nth (dest, l), poly_product (c, List.nth (source, l))))
    in
      List.tabulate (n, fn r => if r = i then newDest else List.nth (a, r))
    end

  (* Find first non-zero entry above diagonal, starting at lower right.
     Returns (~1,~1) if none. *)
  fun find_nonzero (m: i_poly_mat) : int * int =
    let
      val n = length m
      fun entry (i, j) = strip (List.nth (List.nth (m, i), j))
      fun loop (i, j) =
        if i < 0 then
          (~1, ~1)
        else
          let
            val e = entry (i, j)
          in
            if not (isZero e) then
              (i, j)
            else if j + 1 = n then
              loop (i - 1, (i - 1) + 1)
            else
              loop (i, j + 1)
          end
    in
      if n < 2 then (~1, ~1) else loop (n - 2, n - 1)
    end

  (* Internal: for square matrices A,B of size n, compute B * A^{-1}. *)
  fun inv_p (a0: i_poly_mat, b0: i_poly_mat, n: int) : i_poly_mat =
    let
      fun loop (m, nmat) =
        let
          val (i, j) = find_nonzero m
        in
          if i < 0 then
            nmat
          else
            let
              val p = List.nth (List.nth (m, i), j)
              val mp = column_operation (m, i, j, neg p, n)
              val np = column_operation (nmat, i, j, neg p, n)
            in
              loop (mp, np)
            end
        end
    in
      loop (a0, b0)
    end

  fun isUpperTriangular (m: i_poly_mat) : bool =
    let
      val n = length m
      fun rowOk (i, row) =
        List.all (fn j => if j < i then isZero (List.nth (row, j)) else true) (List.tabulate (n, fn j => j))
    in
      List.all rowOk (ListPair.zipEq (List.tabulate (n, fn i => i), m))
    end

  fun hasOnesDiagonal (m: i_poly_mat) : bool =
    let
      val n = length m
      fun diagOk i = (strip (List.nth (List.nth (m, i), i)) = poly_1)
    in
      List.all diagOk (List.tabulate (n, fn i => i))
    end

  (* Inverse of an upper unitriangular polynomial matrix. *)
  fun inverse (m: i_poly_mat) : i_poly_mat =
    let
      val n = length m
      val () = if n = 0 then () else ()
      val () = if List.all (fn r => length r = n) m then () else raise Fail "Inverse.inverse: non-square"
      val () = if isUpperTriangular m then () else raise Fail "Inverse.inverse: not upper triangular"
      val () = if hasOnesDiagonal m then () else raise Fail "Inverse.inverse: diagonal not 1"
    in
      inv_p (m, identity_poly_matrix n, n)
    end

  (* Alternative unitriangular inverse routine (ported from `unitri_inv`). *)
  fun unitri_inv (m: i_poly_mat) : i_poly_mat =
    let
      val n = length m
      val () = if List.all (fn r => length r = n) m then () else raise Fail "Inverse.unitri_inv: non-square"
      val () = if isUpperTriangular m then () else raise Fail "Inverse.unitri_inv: not upper triangular"
      val () = if hasOnesDiagonal m then () else raise Fail "Inverse.unitri_inv: diagonal not 1"

      fun computeRow i : i_poly list =
        let
          val rowRef = ref (List.nth (m, i))
          fun atRow j = List.nth (!rowRef, j)
          fun updateWith (coef: i_poly, useRow: i_poly list) =
            rowRef := List.tabulate (n, fn l => sub (List.nth (!rowRef, l), mul (coef, List.nth (useRow, l))))

          fun entry j =
            if j <= i then
              if j < i then poly_0 else poly_1
            else
              let
                val coef = atRow j
                val () = if isZero coef then () else updateWith (coef, List.nth (m, j))
              in
                neg coef
              end
        in
          List.tabulate (n, entry)
        end
    in
      List.tabulate (n, computeRow)
    end
end

