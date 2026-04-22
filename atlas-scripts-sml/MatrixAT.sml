use "atlas-scripts-sml/IntMatrix.sml";

structure MatrixAT = struct
  type mat = IntMatrix.mat

  fun null (nRows: int, nCols: int) : mat =
    if nRows < 0 orelse nCols < 0 then
      raise Fail "MatrixAT.null: negative dimensions"
    else
      List.tabulate (nRows, fn _ => List.tabulate (nCols, fn _ => 0))

  fun id_mat (n: int) : mat =
    if n < 0 then raise Fail "MatrixAT.id_mat: negative size" else IntMatrix.identity n

  fun block_matrix (a: mat, b: mat) : mat =
    let
      val (ra, ca) = IntMatrix.matShape a
      val (rb, cb) = IntMatrix.matShape b
      val top = List.map (fn row => row @ List.tabulate (cb, fn _ => 0)) a
      val bot = List.map (fn row => List.tabulate (ca, fn _ => 0) @ row) b
    in
      top @ bot
    end

  fun is_permutation (pi: int list) : bool =
    let
      val n = length pi
      val seen = Array.array (n, false)
      fun ok [] = true
        | ok (x :: xs) =
            if x < 0 orelse x >= n then false
            else if Array.sub (seen, x) then false
            else (Array.update (seen, x, true); ok xs)
    in
      ok pi
    end

  fun permutation_matrix (pi: int list) : mat =
    if not (is_permutation pi) then
      raise Fail "MatrixAT.permutation_matrix: not a permutation"
    else
      let
        val n = length pi
        fun row i = List.tabulate (n, fn j => if List.nth (pi, j) = i then 1 else 0)
      in
        List.tabulate (n, row)
      end

  fun gcdInt (a: int, b: int) : int =
    let
      val a = Int.abs a
      val b = Int.abs b
      fun loop (x, 0) = x
        | loop (x, y) = loop (y, x mod y)
    in
      if a = 0 then b else loop (a, b)
    end

  fun gcdList xs =
    (case xs of
       [] => 0
     | x :: rest => List.foldl gcdInt (Int.abs x) rest)

  fun gcdIntInf (a: IntInf.int, b: IntInf.int) : IntInf.int =
    let
      val a = IntInf.abs a
      val b = IntInf.abs b
      fun loop (x, 0) = x
        | loop (x, y) = loop (y, IntInf.mod (x, y))
    in
      if a = 0 then b else loop (a, b)
    end

  fun lcmInt (a: int, b: int) : int =
    let
      val a' = IntInf.fromInt a
      val b' = IntInf.fromInt b
      val g = gcdIntInf (a', b')
      val l = IntInf.div (IntInf.abs (a' * b'), g)
    in
      IntInf.toInt l
    end

  fun lcmList xs =
    List.foldl (fn (x, acc) => if acc = 0 then Int.abs x else lcmInt (acc, x)) 0 xs

  fun diagonalLeftInverse (diag: int list, d: int, nRows: int) : mat =
    let
      val m = length diag
      fun row i =
        List.tabulate
          ( nRows
          , fn j =>
              if i = j andalso i < m then
                d div List.nth (diag, i)
              else
                0
          )
    in
      List.tabulate (m, row)
    end

  (* Port of `weak_left_inverse` from `atlas-scripts/matrix.at`:
     returns (J,d) such that J*A = d*I if A is injective. *)
  fun weak_left_inverse (a: mat) : mat * int =
    let
      val (_, nCols) = IntMatrix.matShape a
      val () =
        if nCols = 0 then
          raise Fail "MatrixAT.weak_left_inverse: unimplemented for 0-column matrices"
        else
          ()
      val (diag, row, col) = IntMatrix.diagonalize a
      val (nRows, _) = IntMatrix.matShape row
      val () =
        if length diag <> nCols then
          raise Fail "MatrixAT.weak_left_inverse: not injective"
        else
          ()
      val () = if List.all (fn x => x <> 0) diag then () else raise Fail "MatrixAT.weak_left_inverse: rank deficient"
      val d = lcmList (List.map Int.abs diag)
      val () = if d = 0 then raise Fail "MatrixAT.weak_left_inverse: zero lcm" else ()
      val dinv = diagonalLeftInverse (diag, d, nRows)
      val j = IntMatrix.matMul (col, IntMatrix.matMul (dinv, row))
    in
      (j, d)
    end

  fun left_inverse (a: mat) : mat =
    let
      val (j, d) = weak_left_inverse a
    in
      if d = 1 then j else raise Fail "MatrixAT.left_inverse: image is not saturated"
    end

  (* Port of `factor_scalar` from `atlas-scripts/matrix.at`. *)
  fun factor_scalar (m: mat) : int * mat =
    let
      val entries = List.concat m
      val d = gcdList entries
    in
      if d = 0 orelse d = 1 then (d, m) else (d, List.map (fn row => List.map (fn x => x div d) row) m)
    end

  fun principal_submatrix (m: mat, s: int list) : mat =
    let
      val (n, k) = IntMatrix.matShape m
      val () = if n = k then () else raise Fail "MatrixAT.principal_submatrix: non-square"
      val () =
        if List.all (fn i => 0 <= i andalso i < n) s then () else raise Fail "MatrixAT.principal_submatrix: index out of range"
      fun entry (i: int, j: int) = List.nth (List.nth (m, i), j)
      fun row r = List.map (fn c => entry (List.nth (s, r), List.nth (s, c))) (List.tabulate (length s, fn i => i))
    in
      List.tabulate (length s, row)
    end

  fun main_diagonal_square_block (m: mat, size: int, offset: int) : mat =
    if size < 0 orelse offset < 0 then
      raise Fail "MatrixAT.main_diagonal_square_block: negative"
    else
      let
        val (n, k) = IntMatrix.matShape m
        val () = if n = k then () else raise Fail "MatrixAT.main_diagonal_square_block: non-square"
        val () = if offset + size <= n then () else raise Fail "MatrixAT.main_diagonal_square_block: oob"
        val s = List.tabulate (size, fn i => offset + i)
      in
        principal_submatrix (m, s)
      end

  fun top_left_square_block (m: mat, size: int) : mat =
    main_diagonal_square_block (m, size, 0)

  fun leading_principal_minor (m: mat, size: int) : mat =
    top_left_square_block (m, size)

  (* Port of `row_echelon` from `atlas-scripts/matrix.at` (via transpose). *)
  fun row_echelon (m: mat) : mat =
    let
      val (e, _, _, _) = IntMatrix.echelon (IntMatrix.transpose m)
    in
      IntMatrix.transpose e
    end

  (* Concatenate matrices with the same number of rows (horizontal concatenation). *)
  fun merge_matrices (ms: mat list) : mat =
    (case ms of
       [] => IntMatrix.identity 0
     | m0 :: rest =>
         List.foldl
           (fn (m, acc) =>
              let
                val (na, _) = IntMatrix.matShape acc
                val (nm, _) = IntMatrix.matShape m
                val () = if na = nm then () else raise Fail "MatrixAT.merge_matrices: row mismatch"
              in
                IntMatrix.hcat (acc, m)
              end)
           m0
           rest)

  (* Port of `kronecker_product` / `Kronecker_product` from `atlas-scripts/matrix.at`. *)
  fun kronecker_product (a: mat, b: mat) : mat =
    let
      val (m, n) = IntMatrix.matShape a
      val (p, q) = IntMatrix.matShape b

      fun entryA (i, j) = List.nth (List.nth (a, i), j)
      fun entryB (i, j) = List.nth (List.nth (b, i), j)

      fun row i =
        let
          val rA = i div p
          val rB = i mod p
        in
          List.tabulate
            ( n * q
            , fn j =>
                let
                  val cA = j div q
                  val cB = j mod q
                in
                  entryA (rA, cA) * entryB (rB, cB)
                end
            )
        end
    in
      List.tabulate (m * p, row)
    end

  val Kronecker_product = kronecker_product

  fun matPow (a: mat, k: int) : mat =
    if k < 0 then
      raise Fail "MatrixAT.matPow: negative exponent"
    else
      let
        val (n, m) = IntMatrix.matShape a
        val () = if n = m then () else raise Fail "MatrixAT.matPow: non-square"
        val id = id_mat n
        fun mul (x, y) = IntMatrix.matMul (x, y)
        fun pow (base, exp, acc) =
          if exp = 0 then acc
          else if exp mod 2 = 1 then pow (mul (base, base), exp div 2, mul (acc, base))
          else pow (mul (base, base), exp div 2, acc)
      in
        pow (a, k, id)
      end

  (* Order of a finite-order square integer matrix, by repeated powering. *)
  fun order (a: mat) : int =
    let
      val (n, m) = IntMatrix.matShape a
      val () = if n = m then () else raise Fail "MatrixAT.order: non-square"
      val id = id_mat n
      val maxIter = 4096
      fun loop (k: int, p: mat) =
        if p = id then k
        else if k >= maxIter then raise Fail "MatrixAT.order: exceeded iteration limit"
        else loop (k + 1, IntMatrix.matMul (p, a))
    in
      if n = 0 then 1 else loop (1, a)
    end

  (* Port of `weak_right_inverse` from `atlas-scripts/matrix.at`:
     returns (B,d) such that A*B = d*I if A is surjective onto a finite-index sublattice. *)
  fun weak_right_inverse (a: mat) : mat * int =
    let
      val (j, d) = weak_left_inverse (IntMatrix.transpose a)
    in
      (IntMatrix.transpose j, d)
    end

  (* Port of `right_inverse` from `atlas-scripts/matrix.at`:
     returns B such that A*B = I if A is surjective as a lattice map. *)
  fun right_inverse (a: mat) : mat =
    IntMatrix.transpose (left_inverse (IntMatrix.transpose a))
end
