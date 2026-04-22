use "atlas-scripts-sml/IntMatrix.sml";

structure MatrixAT = struct
  type mat = IntMatrix.mat

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
end
