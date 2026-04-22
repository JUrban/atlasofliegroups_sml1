use "atlas-scripts-sml/IntMatrix.sml";
use "atlas-scripts-sml/MatReduc.sml";
use "atlas-scripts-sml/Lattice.sml";

(* Small compatibility layer for translating `.at` scripts that depend on
   `lattice.at` names into SML. *)
structure LatticeAT = struct
  type vec = Lattice.vec
  type mat = IntMatrix.mat
  type ratvec = Lattice.ratvec

  val kernel = IntMatrix.kernel
  val eigen_lattice = IntMatrix.eigenLattice

  val in_lattice_basis = MatReduc.inLatticeBasis
  val adapted_basis = MatReduc.adaptedBasis

  val vec_solve = Lattice.vec_solve

  fun matColumns (a: mat) : vec list =
    let
      val (_, m) = IntMatrix.matShape a
      fun col j = List.map (fn row => List.nth (row, j)) a
    in
      List.tabulate (m, col)
    end

  fun matFromColumns (cols: vec list) : mat =
    (case cols of
       [] => []
     | c0 :: cs =>
         let
           val n = length c0
           val () = if List.all (fn c => length c = n) cs then () else raise Fail "LatticeAT.matFromColumns: ragged"
           fun row i = List.map (fn c => List.nth (c, i)) cols
         in
           List.tabulate (n, row)
         end)

  fun matFromColumnsN (nRows: int, cols: vec list) : mat =
    if nRows < 0 then
      raise Fail "LatticeAT.matFromColumnsN: negative row count"
    else if null cols then
      List.tabulate (nRows, fn _ => [])
    else
      let
        val m = matFromColumns cols
        val (r, _) = IntMatrix.matShape m
        val () = if r = nRows then () else raise Fail "LatticeAT.matFromColumnsN: row mismatch"
      in
        m
      end

  fun solve_mat (a: mat, b: mat) : mat option =
    let
      val (na, _) = IntMatrix.matShape a
      val (nb, mb) = IntMatrix.matShape b
      val () = if na = nb then () else raise Fail "LatticeAT.solve_mat: row mismatch"
      val (_, ma) = IntMatrix.matShape a
      val bcols = matColumns b
      fun loop ([], acc) = SOME (matFromColumns (List.rev acc))
        | loop (v :: vs, acc) =
            (case Lattice.solve (a, v) of
               NONE => NONE
             | SOME x => loop (vs, x :: acc))
    in
      if mb = 0 then SOME (List.tabulate (ma, fn _ => [])) else loop (bcols, [])
    end

  (* Port of `restrict_action(A,M)` from `atlas-scripts/lattice.at`. *)
  fun restrict_action (a: mat, m: mat) : mat =
    let
      val rhs = IntMatrix.matMul (a, m)
    in
      case solve_mat (m, rhs) of
        NONE => raise Fail "LatticeAT.restrict_action: image lattice not fixed by A"
      | SOME b => b
    end

  (* Port of `corestrict_action(M,A)` from `atlas-scripts/lattice.at`. *)
  fun corestrict_action (m: mat, a: mat) : mat =
    let
      val mt = IntMatrix.transpose m
      val at = IntMatrix.transpose a
      val rhs = IntMatrix.matMul (at, mt)
    in
      case solve_mat (mt, rhs) of
        NONE => raise Fail "LatticeAT.corestrict_action: kernel not fixed by A"
      | SOME bt => IntMatrix.transpose bt
    end

  fun intersection_plus (a: mat, b: mat) : mat * mat =
    let
      val (na, ca) = IntMatrix.matShape a
      val (nb, cb) = IntMatrix.matShape b
      val () = if na = nb then () else raise Fail "LatticeAT.intersection_plus: row mismatch"
    in
      if ca = 0 orelse cb = 0 then
        (List.tabulate (na, fn _ => []), List.tabulate (0, fn _ => []))
      else
        let
          val m = IntMatrix.hcat (a, IntMatrix.neg b) (* n x (ca+cb) *)
          val nker = IntMatrix.kernel m (* (ca+cb) x t; columns are solutions *)
          val y = IntMatrix.firstRows (ca, nker) (* ca x t *)
          val x = IntMatrix.matMul (a, y) (* n x t *)
        in
          (x, y)
        end
    end

  fun intersection (a: mat, b: mat) : mat =
    let
      val (x, _) = intersection_plus (a, b)
    in
      x
    end

  (* Port of `image_lattice` / `image_lattice_plus` from `atlas-scripts/lattice_aux.at`. *)
  fun image_lattice_plus (a: mat) : mat * mat =
    let
      val (e, c0, _, _) = IntMatrix.echelon a
      val (_, nColsA) = IntMatrix.matShape a
      val (nRowsE, nColsE) = IntMatrix.matShape e
      val (c0r, c0c) = IntMatrix.matShape c0
      val () =
        if c0r = nColsA andalso c0c = nColsA then () else raise Fail "LatticeAT.image_lattice_plus: bad C0 size"
      val c = IntMatrix.firstCols (nColsE, c0) (* nColsA x nColsE *)

      val colsE = matColumns e
      val colsC = matColumns c

      fun pivotIndex col =
        let
          fun loop ([], _, last) = last
            | loop (x :: xs, i, last) = loop (xs, i + 1, if x <> 0 then SOME i else last)
        in
          loop (col, 0, NONE)
        end

      fun entry (col: vec, i: int) = List.nth (col, i)
      fun colSubMul (col: vec, pivotCol: vec, q: int) : vec =
        ListPair.mapEq (fn (x, y) => x - q * y) (col, pivotCol)

      fun reduceAt (j: int, (eCols, cCols)) =
        let
          val pivotColE = List.nth (eCols, j)
          val pivotColC = List.nth (cCols, j)
        in
          case pivotIndex pivotColE of
            NONE => (eCols, cCols)
          | SOME i =>
              let
                val pivot = entry (pivotColE, i)
                val () = if pivot > 0 then () else raise Fail "LatticeAT.image_lattice_plus: nonpositive pivot"

                fun upd (l: int, (eAcc, cAcc)) =
                  if l <= j then (eAcc, cAcc)
                  else
                    let
                      val colE = List.nth (eAcc, l)
                      val q = entry (colE, i) div pivot
                      val colE' = if q = 0 then colE else colSubMul (colE, pivotColE, q)
                      val colC = List.nth (cAcc, l)
                      val colC' = if q = 0 then colC else colSubMul (colC, pivotColC, q)

                      fun replaceAt (xs, idx, v) = List.take (xs, idx) @ (v :: List.drop (xs, idx + 1))
                      val eAcc' = replaceAt (eAcc, l, colE')
                      val cAcc' = replaceAt (cAcc, l, colC')
                    in
                      (eAcc', cAcc')
                    end

                fun loop l acc =
                  if l >= nColsE then acc else loop (l + 1) (upd (l, acc))
              in
                loop (j + 1) (eCols, cCols)
              end
        end

      fun loopJ j acc =
        if j >= nColsE then acc else loopJ (j + 1) (reduceAt (j, acc))

      val (eCols2, cCols2) = loopJ 0 (colsE, colsC)
    in
      (matFromColumnsN (nRowsE, eCols2), matFromColumnsN (nColsA, cCols2))
    end

  fun image_lattice (a: mat) : mat =
    let
      val (m, _) = image_lattice_plus a
    in
      m
    end
end
