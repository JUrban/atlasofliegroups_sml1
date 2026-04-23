use "atlas-scripts-sml/BigRat.sml";

(*
  File: atlas-scripts-sml/Gaussian_elim_Jeff.sml

  Purpose
  - SML translation of `atlas-scripts/Gaussian_elim_Jeff.at`.
  - Provides a simple (non-optimized) rational Gaussian elimination toolkit
    operating on “matrices by columns” (a list of column vectors).

  Status
  - This file is mainly kept for testing/comparison; the primary elimination
    code used by the SML ports lives elsewhere (`Gaussian_elim.sml`,
    `IntMatrix.sml`, etc.).

  Representation
  - `rat` is `BigRat.t` (exact rationals over `IntInf.int`).
  - A matrix `ratmat` is represented as a list of columns:
      `ratmat = rat list list` where each inner list is a column, and all
      columns have the same length (the number of rows).

  Ownership / side effects
  - All operations are pure (no mutation observable to callers).
*)

structure Gaussian_elim_Jeff = struct
  type rat = BigRat.t
  type ratvec = rat list
  type ratmat = ratvec list (* by columns *)

  fun rat0 () : rat = BigRat.zero ()
  fun rat1 () : rat = BigRat.one ()
  fun ratNeg q = BigRat.neg q
  fun ratAdd (a, b) = BigRat.add (a, b)
  fun ratSub (a, b) = BigRat.sub (a, b)
  fun ratMul (a, b) = BigRat.mul (a, b)
  fun ratDiv (a, b) = BigRat.divRat (a, b)

  fun ratAbs (q: rat) : rat =
    let
      val q = BigRat.normalize q
      val n = IntInf.abs (#num q)
    in
      {num = n, den = #den q}
    end

  fun ratCompare (a: rat, b: rat) : order =
    let
      val a = BigRat.normalize a
      val b = BigRat.normalize b
      val lhs = #num a * #den b
      val rhs = #num b * #den a
    in
      IntInf.compare (lhs, rhs)
    end

  fun ratCompareAbs (a: rat, b: rat) : order = ratCompare (ratAbs a, ratAbs b)

  (* List utilities *)
  fun nth (xs: 'a list, i: int) : 'a =
    List.nth (xs, i)

  fun updateAt (xs: 'a list, i: int, v: 'a) : 'a list =
    let
      fun loop ([], _, _) = raise Subscript
        | loop (x :: rest, 0, acc) = List.rev acc @ (v :: rest)
        | loop (x :: rest, k, acc) = loop (rest, k - 1, x :: acc)
    in
      if i < 0 then raise Subscript else loop (xs, i, [])
    end

  fun length0 (xs: 'a list) : int = length xs

  (* Convert list-of-columns into an array-of-arrays for efficient updates. *)
  fun toArrayMat (m: ratmat) : rat Array.array Array.array =
    Array.fromList (List.map Array.fromList m)

  fun fromArrayMat (a: rat Array.array Array.array) : ratmat =
    Array.foldr (fn (col, acc) => Array.foldr op:: [] col :: acc) [] a

  fun shape (m: ratmat) : int * int =
    case m of
      [] => (0, 0)
    | col0 :: _ => (length m, length col0)

  fun assertRect (m: ratmat) : unit =
    (case m of
       [] => ()
     | col0 :: cols =>
         let
           val r = length col0
           val ok = List.all (fn c => length c = r) cols
         in
           if ok then () else raise Fail "Gaussian_elim_Jeff: non-rectangular matrix"
         end)

  (* Find pivot row index in `v[k..]` maximizing absolute value. *)
  fun find_pivot (k: int, v: ratvec) : int =
    let
      val n = length v
      val () = if k < n then () else raise Fail "Gaussian_elim_Jeff.find_pivot: empty tail"
      fun loop (i, bestI, bestVal) =
        if i = n then
          bestI
        else
          let
            val cur = nth (v, i)
          in
            if ratCompareAbs (cur, bestVal) = GREATER then
              loop (i + 1, i, cur)
            else
              loop (i + 1, bestI, bestVal)
          end
    in
      loop (k + 1, k, nth (v, k))
    end

  (* Swap rows i and k in a column-matrix. *)
  fun swap_rows (m: ratmat, i: int, k: int) : ratmat =
    let
      fun swapCol col =
        let
          val a = nth (col, i)
          val b = nth (col, k)
          val col = updateAt (col, i, b)
          val col = updateAt (col, k, a)
        in
          col
        end
    in
      List.map swapCol m
    end

  (*
    Gaussian elimination (row operations) to put a nonsingular rational matrix
    into an upper-triangular form.
  *)
  fun gauss (m: ratmat) : ratmat =
    let
      val () = assertRect m
      val (nCols, nRows) = shape m
      val steps = Int.min (nCols, nRows)
      val a = toArrayMat m

      fun col j = Array.sub (a, j)
      fun get (j, i) = Array.sub (col j, i)
      fun set (j, i, v) = Array.update (col j, i, v)

      fun swapRowsInPlace (i: int, k: int) : unit =
        let
          fun loop j =
            if j = nCols then
              ()
            else
              let
                val tmp = get (j, i)
                val () = set (j, i, get (j, k))
                val () = set (j, k, tmp)
              in
                loop (j + 1)
              end
        in
          if i = k then () else loop 0
        end

      fun pivotRow (k: int) : int =
        let
          val mk = Array.foldr op:: [] (col k)
        in
          find_pivot (k, mk)
        end

      fun elimAt (k: int) : unit =
        if k = steps then
          ()
        else
          let
            val pr = pivotRow k
            val () = if pr > k then swapRowsInPlace (k, pr) else ()
            val pivot = get (k, k)
            fun loopI i =
              if i = nRows then
                ()
              else
                let
                  val r = ratDiv (get (k, i), pivot)
                  fun loopJ j =
                    if j = nCols then
                      ()
                    else
                      (if j >= k then set (j, i, ratSub (get (j, i), ratMul (r, get (j, k)))) else ();
                       loopJ (j + 1))
                in
                  loopJ 0;
                  loopI (i + 1)
                end
          in
            loopI (k + 1);
            elimAt (k + 1)
          end
    in
      elimAt 0;
      fromArrayMat a
    end

  (* Solve Mx=y with M upper-triangular, square, nonsingular. *)
  fun back_solve (m: ratmat, y: ratvec) : ratvec =
    let
      val () = assertRect m
      val (nCols, nRows) = shape m
      val () = if nCols = nRows andalso length y = nRows then () else raise Fail "Gaussian_elim_Jeff.back_solve: shape mismatch"
      val a = toArrayMat m
      val rv = Array.array (nRows, rat0 ())
      fun get (j, i) = Array.sub (Array.sub (a, j), i)

      fun loopK k =
        if k < 0 then
          ()
        else
          let
            fun sumLoop (j, acc) =
              if j = nRows then
                acc
              else
                sumLoop (j + 1, ratAdd (acc, ratMul (get (j, k), Array.sub (rv, j))))
            val sum = sumLoop (k + 1, rat0 ())
            val rhs = ratSub (nth (y, k), sum)
            val xk = ratDiv (rhs, get (k, k))
            val () = Array.update (rv, k, xk)
          in
            loopK (k - 1)
          end
    in
      loopK (nRows - 1);
      Array.foldr op:: [] rv
    end

  (* Matrix-vector multiply (matrix by columns). *)
  fun times (m: ratmat, v: ratvec) : ratvec =
    let
      val () = assertRect m
      val (nCols, nRows) = shape m
      val () = if length v = nCols then () else raise Fail "Gaussian_elim_Jeff.times: size mismatch"
      fun entry i =
        let
          fun loop (j, acc) =
            if j = nCols then
              acc
            else
              loop (j + 1, ratAdd (acc, ratMul (nth (nth (m, j), i), nth (v, j))))
        in
          loop (0, rat0 ())
        end
    in
      List.tabulate (nRows, entry)
    end

  (* Solve Mx=y for nonsingular square matrix M, using gaussian elimination. *)
  fun solve (m: ratmat, y: ratvec) : ratvec =
    let
      val () = assertRect m
      val (nCols, nRows) = shape m
      val () = if nCols = nRows andalso length y = nRows then () else raise Fail "Gaussian_elim_Jeff.solve: shape mismatch"
      val aug = m @ [y]
      val a = gauss aug
      val m2 = List.take (a, length a - 1)
      val z = List.nth (a, length a - 1)
    in
      back_solve (m2, z)
    end
end

