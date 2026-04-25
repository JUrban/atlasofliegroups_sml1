use "atlas-scripts-sml/BigRat.sml";
use "atlas-scripts-sml/IntMatrix.sml";

(*
  File: atlas-scripts-sml/Smith.sml

  Purpose
  - Standard ML translation of `atlas-scripts/Smith.at`.
  - Provides “big integer only” versions of a handful of matrix routines that
    the Atlas `.at` scripting environment exposes as built-ins (Smith-style /
    echelon-style helpers), implemented directly in SML using `IntInf.int`.

  Why this exists
  - Many Atlas `.at` scripts implicitly assume that the scripting-language
    `int` is unbounded, so routines like Bezout-style elimination are written
    without overflow concerns.
  - In Poly/ML, `int` is bounded; this port therefore uses `IntInf.int` for
    all integer linear algebra in this module.

  Representation (matching the `.at` file)
  - `Mat` is stored *by columns*:
      - `type Vec = IntInf.int list`
      - `type Mat = Vec list`
    so `M[j][i]` is entry (row i, column j), as in the `.at` script.
  - Rational matrices use `BigRat.t` and are also stored by columns.

  Limitations / notes
  - The `.at` file defines a full `diagonalise` algorithm in terms of repeated
    row/column Euclidean reduction. Re-implementing that algorithm verbatim in
    SML would be quite long and error-prone, and the current development rarely
    needs it.
  - This port therefore provides `diagonalise` via the existing C++ shim
    `IntMatrix.diagonalize`, which operates on bounded `int` matrices; we only
    support matrices whose entries fit in SML `int`.
    - If you need true big-integer diagonalisation in SML, replace
      `diagonalise` with a faithful port of the `.at` algorithm (kept in the
      source `atlas-scripts/Smith.at`).
*)

structure Smith = struct
  type Vec = IntInf.int list
  type Mat = Vec list (* by columns *)

  type Rat = BigRat.t
  type RatVec = Rat list
  type RatMat = RatVec list (* by columns *)

  fun fail where' msg = raise Fail ("Smith." ^ where' ^ ": " ^ msg)

  fun absI (x: IntInf.int) = IntInf.abs x

  fun divModPos (a: IntInf.int, d: IntInf.int) : IntInf.int * IntInf.int =
    if d <= 0 then fail "divModPos" "expected positive divisor"
    else (IntInf.div (a, d), IntInf.mod (a, d))

  fun vecLen (v: 'a list) = length v
  fun matCols (m: Mat) = length m
  fun matRows (m: Mat) = if null m then 0 else length (hd m)

  fun requireRectMat (where': string, m: Mat) =
    (case m of
       [] => ()
     | c0 :: cs =>
         let
           val r = length c0
           val () =
             if List.all (fn c => length c = r) cs then ()
             else fail where' "ragged Mat (column lengths differ)"
         in
           ()
         end)

  fun requireRectRatMat (where': string, m: RatMat) =
    (case m of
       [] => ()
     | c0 :: cs =>
         let
           val r = length c0
           val () =
             if List.all (fn c => length c = r) cs then ()
             else fail where' "ragged RatMat (column lengths differ)"
         in
           ()
         end)

  fun id_mat (n: int) : Mat =
    List.tabulate
      (n, fn j => List.tabulate (n, fn i => IntInf.fromInt (if i = j then 1 else 0)))

  fun minus (col: Vec) : Vec = List.map (fn e => ~e) col
  fun minusRat (col: RatVec) : RatVec = List.map BigRat.neg col

  fun vecSubScaled (c1: Vec, c0: Vec, f: IntInf.int) : Vec =
    ListPair.mapEq (fn (e, e0) => e - e0 * f) (c1, c0)

  fun ratVecSubScaled (c1: RatVec, c0: RatVec, f: Rat) : RatVec =
    ListPair.mapEq (fn (e, e0) => BigRat.sub (e, BigRat.mul (e0, f))) (c1, c0)

  (* `list(n,pred)` from `basic.at`: indices `0..n-1` satisfying pred. *)
  fun indices (n: int, pred: int -> bool) : int list =
    let
      fun loop (i, acc) =
        if i = n then List.rev acc
        else loop (i + 1, if pred i then i :: acc else acc)
    in
      loop (0, [])
    end

  fun abs_mindex_vec (a: Vec) : int =
    let
      fun loop (_, [], bestI, bestAbs) = bestI
        | loop (i, x :: xs, bestI, bestAbs) =
            if x = 0 then loop (i + 1, xs, bestI, bestAbs)
            else
              let
                val ax = absI x
              in
                if bestI = ~1 orelse ax < bestAbs
                then loop (i + 1, xs, i, ax)
                else loop (i + 1, xs, bestI, bestAbs)
              end
    in
      loop (0, a, ~1, 0)
    end

  (* Find position of minimal nonzero absolute entry in submatrix `M[i0:, j0:]`. *)
  fun abs_mindex (m: Mat, i0: int, j0: int) : int * int =
    let
      val () = requireRectMat ("abs_mindex", m)
      val c = matCols m
      val r = matRows m
      val () =
        if 0 <= i0 andalso i0 <= r andalso 0 <= j0 andalso j0 <= c then ()
        else fail "abs_mindex" "start indices out of range"
      fun entry (j, i) = List.nth (List.nth (m, j), i)
      fun loopCols (j, best) =
        if j = c then best
        else
          let
            fun loopRows (i, best') =
              if i = r then best'
              else
                let
                  val e = entry (j, i)
                in
                  if e = 0 then loopRows (i + 1, best')
                  else
                    (case best' of
                       NONE => loopRows (i + 1, SOME (i, j, absI e))
                     | SOME (_, _, mabs) =>
                         let val ae = absI e in
                           if ae < mabs then loopRows (i + 1, SOME (i, j, ae))
                           else loopRows (i + 1, best')
                         end)
                end
          in
            loopCols (j + 1, loopRows (i0, best))
          end
      val best = loopCols (j0, NONE)
    in
      case best of
        NONE => (i0, j0) (* matches `.at`: returns default when none found *)
      | SOME (i, j, _) => (i, j)
    end

  (*
    `.at`: Bezout(Vec a) = (gcd, M) where columns of M satisfy a*M = (d,0,...).
    This is a literal translation of the `.at` loop structure, using arrays.
  *)
  fun Bezout (a0: Vec) : IntInf.int * Mat =
    let
      val n = vecLen a0
      val a = Array.fromList a0
      val m = Array.fromList (List.tabulate (n, fn j =>
        Array.fromList (List.tabulate (n, fn i => IntInf.fromInt (if i = j then 1 else 0)))))

      fun colNeg (j: int) =
        let
          val col = Array.sub (m, j)
          val col' = Array.tabulate (Array.length col, fn i => ~ (Array.sub (col, i)))
        in
          Array.update (m, j, col')
        end

      fun colSubScaledArr (j: int, cur: int, q: IntInf.int) =
        let
          val cj = Array.sub (m, j)
          val ccur = Array.sub (m, cur)
          val len = Array.length cj
          val cj' = Array.tabulate (len, fn i => Array.sub (cj, i) - Array.sub (ccur, i) * q)
        in
          Array.update (m, j, cj')
        end

      fun swapCols (j0: int, j1: int) =
        if j0 = j1 then ()
        else
          let
            val t = Array.sub (m, j0)
            val () = Array.update (m, j0, Array.sub (m, j1))
            val () = Array.update (m, j1, t)
          in
            ()
          end

      val non_0 = indices (n, fn i => Array.sub (a, i) <> 0)
    in
      if null non_0 then
        (0, List.tabulate (n, fn j => Array.foldr (op ::) [] (Array.sub (m, j))))
      else
        let
          fun minAbsIdx (is: int list) =
            let
              fun loop ([], bestI, bestAbs) = bestI
                | loop (i :: rest, bestI, bestAbs) =
                    let
                      val ai = absI (Array.sub (a, i))
                    in
                      if bestI = ~1 orelse ai < bestAbs then loop (rest, i, ai)
                      else loop (rest, bestI, bestAbs)
                    end
            in
              loop (is, ~1, 0)
            end

          val mindex0 = minAbsIdx non_0
          val () =
            if Array.sub (a, mindex0) < 0 then
              (Array.update (a, mindex0, ~ (Array.sub (a, mindex0)));
               colNeg mindex0)
            else
              ()

          fun loop (non0: int list, mindex: int) : int list * int =
            if length non0 <= 1 then (non0, mindex)
            else
              let
                val min0 = Array.sub (a, mindex)
                val cur_i = mindex
                val d = min0
                fun step (i, acc, mindexAcc, minAcc) =
                  if i = cur_i then (i :: acc, mindexAcc, minAcc)
                  else
                    let
                      val (q, r) = divModPos (Array.sub (a, i), d)
                      val () = colSubScaledArr (i, cur_i, q)
                      val () = Array.update (a, i, r)
                    in
                      if r = 0 then (acc, mindexAcc, minAcc)
                      else if r < minAcc then (i :: acc, i, r)
                      else (i :: acc, mindexAcc, minAcc)
                    end

                val (non0', mindex', _) =
                  List.foldl
                    (fn (i, (acc, mi, mn)) =>
                       let val (acc', mi', mn') = step (i, acc, mi, mn)
                       in (acc', mi', mn') end)
                    ([], mindex, min0)
                    non0
              in
                loop (List.rev non0', mindex')
              end

          val (_, mindex1) = loop (non_0, mindex0)
          val () = if mindex1 > 0 then swapCols (0, mindex1) else ()
          val d = Array.sub (a, mindex1)
          val mout = List.tabulate (n, fn j => Array.foldr (op ::) [] (Array.sub (m, j)))
        in
          (d, mout)
        end
    end

  (* Safe (no-overflow) multiplication in column-major format. *)
  fun safe_prod_mat_vec (a: Mat, b: Vec) : Vec =
    let
      val () = requireRectMat ("safe_prod_mat_vec", a)
      val n = matCols a
      val m = matRows a
      val () = if n = vecLen b then () else fail "safe_prod_mat_vec" "size mismatch"
      fun row i = List.map (fn col => List.nth (col, i)) a
      val rows = List.tabulate (m, row)
      fun dot (xs, ys) =
        List.foldl (op +) 0 (ListPair.mapEq (op * ) (xs, ys))
    in
      List.map (fn r => dot (r, b)) rows
    end

  fun safe_prod_mat_mat (a: Mat, b: Mat) : Mat =
    let
      val () = requireRectMat ("safe_prod_mat_mat", a)
      val () = requireRectMat ("safe_prod_mat_mat", b)
      val () =
        if matCols a = matRows b then ()
        else fail "safe_prod_mat_mat" "size mismatch"
    in
      List.map (fn col => safe_prod_mat_vec (a, col)) b
    end

  fun bigRatMulIntInf (q: Rat, k: IntInf.int) : Rat =
    let
      val q = BigRat.normalize q
    in
      BigRat.normalize {num = #num q * k, den = #den q}
    end

  fun safe_prod_ratmat_ratvec (a: RatMat, b: RatVec) : RatVec =
    let
      val () = requireRectRatMat ("safe_prod_ratmat_ratvec", a)
      val n = length a
      val m = if null a then 0 else length (hd a)
      val () = if n = length b then () else fail "safe_prod_ratmat_ratvec" "size mismatch"
      fun row i = List.map (fn col => List.nth (col, i)) a
      val rows = List.tabulate (m, row)
      fun dot (xs, ys) =
        List.foldl BigRat.add (BigRat.zero ())
          (ListPair.mapEq (fn (x, y) => BigRat.mul (x, y)) (xs, ys))
    in
      List.map (fn r => dot (r, b)) rows
    end

  fun safe_prod_ratmat_ratmat (a: RatMat, b: RatMat) : RatMat =
    let
      val () = requireRectRatMat ("safe_prod_ratmat_ratmat", a)
      val () = requireRectRatMat ("safe_prod_ratmat_ratmat", b)
      val () =
        if length a = (if null b then 0 else length (hd b)) then ()
        else fail "safe_prod_ratmat_ratmat" "size mismatch"
    in
      List.map (fn col => safe_prod_ratmat_ratvec (a, col)) b
    end

  (* Column echelon form (big-int version, following `Smith.at`). *)
  fun column_echelon (a0: Mat) : Mat * Mat * int list * bool =
    let
      val () = requireRectMat ("column_echelon", a0)
      val n = matCols a0
      val m = matRows a0
    in
      if n = 0 then (a0, [], [], false)
      else
        let
          val M = Array.fromList (List.map Array.fromList a0)
          val C = Array.fromList (List.tabulate (n, fn j =>
            Array.fromList (List.tabulate (n, fn i => IntInf.fromInt (if i = j then 1 else 0)))))
          val piv = Array.array (m, true)
          val flip = ref false
          val l = ref n

          fun getCol (arrs, j) = Array.sub (arrs, j)
          fun swapArr (arrs, j0, j1) =
            if j0 = j1 then ()
            else
              let
                val t = Array.sub (arrs, j0)
                val () = Array.update (arrs, j0, Array.sub (arrs, j1))
                val () = Array.update (arrs, j1, t)
              in
                ()
              end

          fun colNeg (arrs, j) =
            let
              val col = getCol (arrs, j)
              val col' = Array.tabulate (Array.length col, fn i => ~ (Array.sub (col, i)))
            in
              Array.update (arrs, j, col')
            end

          fun colSubScaled (arrs, j, pivotJ, q) =
            let
              val cj = getCol (arrs, j)
              val cp = getCol (arrs, pivotJ)
              val len = Array.length cj
              val cj' = Array.tabulate (len, fn i => Array.sub (cj, i) - Array.sub (cp, i) * q)
            in
              Array.update (arrs, j, cj')
            end

          fun entry (j, i) = Array.sub (getCol (M, j), i)

          fun csRow (i: int) : int list =
            indices (!l, fn j => entry (j, i) <> 0)

          fun chooseMinAbs (i: int, cs: int list) : int =
            let
              fun loop ([], bestJ, bestAbs) = bestJ
                | loop (j :: rest, bestJ, bestAbs) =
                    let
                      val aj = absI (entry (j, i))
                    in
                      if bestJ = ~1 orelse aj < bestAbs then loop (rest, j, aj)
                      else loop (rest, bestJ, bestAbs)
                    end
            in
              loop (cs, ~1, 0)
            end

          fun reduceRow (i: int, cs: int list, cc: int) : int list * int =
            if length cs <= 1 then (cs, cc)
            else
              let
                val pivot0 =
                  let
                    val p = entry (cc, i)
                    val () =
                      if p < 0 then (colNeg (M, cc); colNeg (C, cc); flip := not (!flip)) else ()
                  in
                    absI (entry (cc, i))
                  end

                fun step j acc =
                  if j = cc then j :: acc
                  else
                    let
                      val (q, r) = divModPos (entry (j, i), pivot0)
                      val () = colSubScaled (M, j, cc, q)
                      val () = colSubScaled (C, j, cc, q)
                    in
                      if entry (j, i) = 0 then acc else j :: acc
                    end

                val cs' = List.rev (List.foldl (fn (j, acc) => step j acc) [] cs)
                val cc' = chooseMinAbs (i, cs')
              in
                reduceRow (i, cs', cc')
              end

          fun loopRows i =
            if i = m then ()
            else
              let
                val cs = csRow i
              in
                if null cs then
                  (Array.update (piv, i, false); loopRows (i + 1))
                else
                  let
                    val cc0 = chooseMinAbs (i, cs)
                    val (cs1, cc1) = reduceRow (i, cs, cc0)
                    val l1 = !l - 1
                    val () = l := l1
                    val () =
                      if cc1 < l1 then (swapArr (M, cc1, l1); swapArr (C, cc1, l1); flip := not (!flip))
                      else ()
                  in
                    loopRows (i + 1)
                  end
              end

          val () = loopRows 0

          fun arrColsToList (arrs: IntInf.int Array.array Array.array) : Mat =
            List.tabulate (Array.length arrs, fn j => Array.foldr (op ::) [] (Array.sub (arrs, j)))

          val pivRows = indices (m, fn i => Array.sub (piv, i))
          val lfinal = !l
          val Mlist = arrColsToList M
          val Clist = arrColsToList C
        in
          if lfinal = 0 then
            (Mlist, Clist, pivRows, !flip)
          else
            let
              val Mout = List.drop (Mlist, lfinal)
              val Cout = List.drop (Clist, lfinal) @ List.take (Clist, lfinal)
              val flipOut =
                if lfinal mod 2 = 0 then !flip
                else if n mod 2 = 0 then not (!flip)
                else !flip
            in
              (Mout, Cout, pivRows, flipOut)
            end
        end
    end

  fun ech_det (a: Mat) : IntInf.int =
    let
      val (m, _, piv, flip) = column_echelon a
      val n = matCols a
      fun diagEntry i = List.nth (List.nth (m, i), i)
      fun prod (i, acc) = if i = n then acc else prod (i + 1, acc * diagEntry i)
    in
      if length m = length piv andalso length m = n then
        let
          val det = prod (0, 1)
        in
          if flip then ~det else det
        end
      else
        0
    end

  fun ech_solve (a: Mat, b0: Vec) : Vec option =
    let
      val () = requireRectMat ("ech_solve", a)
      val n = matCols a
      val () = if vecLen b0 = n then () else fail "ech_solve" "equation mismatch"
      val (m, c, piv, _) = column_echelon a
      val k = matCols m
      val b = Array.fromList b0
      val sol = Array.array (k, 0 : IntInf.int)

      fun colAt j = List.nth (m, j)

      fun subColScaled (col: Vec, q: IntInf.int) =
        let
          val len = Array.length b
          fun loop i =
            if i = len then ()
            else (Array.update (b, i, Array.sub (b, i) - List.nth (col, i) * q); loop (i + 1))
        in
          loop 0
        end

      fun isPivotRow (i: int, jFromEnd: int) =
        let
          val idx = (length piv - 1) - jFromEnd
        in
          idx >= 0 andalso List.nth (piv, idx) = i
        end

      fun loopRows (i: int, jFromEnd: int) : bool =
        if i < 0 then true
        else if jFromEnd < k andalso isPivotRow (i, jFromEnd) then
          let
            val idx = (k - 1) - jFromEnd
            val col = colAt idx
            val pivot = List.nth (col, i)
            val (q, r) = divModPos (Array.sub (b, i), absI pivot)
            val q = if pivot < 0 then ~q else q
          in
            if r <> 0 then false
            else
              (Array.update (sol, idx, q);
               subColScaled (col, q);
               loopRows (i - 1, jFromEnd + 1))
          end
        else
          if Array.sub (b, i) <> 0 then false else loopRows (i - 1, jFromEnd)

      val ok = loopRows (n - 1, 0)
    in
      if not ok then NONE
      else
        let
          val solList = Array.foldr (op ::) [] sol
          val cFirst = List.take (c, k)
        in
          SOME (safe_prod_mat_vec (cFirst, solList))
        end
    end

  fun ech_solve_rat (a: Mat, b0: RatVec) : RatVec option =
    let
      val () = requireRectMat ("ech_solve_rat", a)
      val n = matCols a
      val () = if length b0 = n then () else fail "ech_solve_rat" "equation mismatch"
      val (m, c, piv, _) = column_echelon a
      val k = matCols m
      val b = Array.fromList b0
      val sol = Array.array (k, BigRat.zero ())

      fun colAt j = List.nth (m, j)

      fun intRat (x: IntInf.int) : Rat = BigRat.make (x, 1)

      fun subColScaled (col: Vec, q: Rat) =
        let
          val len = Array.length b
          fun loop i =
            if i = len then ()
            else
              (Array.update
                 (b, i, BigRat.sub (Array.sub (b, i), bigRatMulIntInf (q, List.nth (col, i))));
               loop (i + 1))
        in
          loop 0
        end

      fun isPivotRow (i: int, jFromEnd: int) =
        let
          val idx = (length piv - 1) - jFromEnd
        in
          idx >= 0 andalso List.nth (piv, idx) = i
        end

      fun loopRows (i: int, jFromEnd: int) : bool =
        if i < 0 then true
        else if jFromEnd < k andalso isPivotRow (i, jFromEnd) then
          let
            val idx = (k - 1) - jFromEnd
            val col = colAt idx
            val pivot = List.nth (col, i)
            val q = BigRat.divRat (Array.sub (b, i), intRat pivot)
          in
            Array.update (sol, idx, q);
            subColScaled (col, q);
            loopRows (i - 1, jFromEnd + 1)
          end
        else
          if not (BigRat.isZero (Array.sub (b, i))) then false else loopRows (i - 1, jFromEnd)

      val ok = loopRows (n - 1, 0)
    in
      if not ok then NONE
      else
        let
          val solList = Array.foldr (op ::) [] sol
          val cFirst = List.take (c, k)
          fun safe_prod_rat_intmat (cols: Mat, xs: RatVec) : RatVec =
            let
              val () = if length cols = length xs then () else fail "ech_solve_rat" "size mismatch"
              val rows =
                List.tabulate
                  (n, fn i => List.map (fn col => intRat (List.nth (col, i))) cols)
              fun dot (rs, ys) =
                List.foldl BigRat.add (BigRat.zero ())
                  (ListPair.mapEq (fn (r, y) => BigRat.mul (r, y)) (rs, ys))
            in
              List.map (fn r => dot (r, xs)) rows
            end
        in
          SOME (safe_prod_rat_intmat (cFirst, solList))
        end
    end

  (* Diagonalisation: delegated to the C++ int-matrix diagonaliser (bounded). *)
  fun diagonalise (m: Mat) : Mat * Vec * Mat =
    let
      val () = requireRectMat ("diagonalise", m)
      val rows =
        List.tabulate (matRows m, fn i => List.map (fn col => List.nth (col, i)) m)

      fun toInt (x: IntInf.int) : int =
        (IntInf.toInt x
         handle _ => fail "diagonalise" "matrix entry does not fit in SML int (C++ diagonaliser is bounded)")

      val rowsInt = List.map (fn r => List.map toInt r) rows
      val (diag, row, col) = IntMatrix.diagonalize rowsInt

      fun colsOfRows rows' : Mat =
        let
          val nrows = length rows'
          val ncols = if nrows = 0 then 0 else length (hd rows')
          fun col j = List.tabulate (nrows, fn i => IntInf.fromInt (List.nth (List.nth (rows', i), j)))
        in
          List.tabulate (ncols, col)
        end

      val L = colsOfRows row
      val R = colsOfRows col
      val d = List.map IntInf.fromInt diag
    in
      (L, d, R)
    end
end
