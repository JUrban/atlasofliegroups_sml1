use "atlas-scripts-sml/IntMatrix.sml";
use "atlas-scripts-sml/MatReduc.sml";
use "atlas-scripts-sml/Lattice.sml";
use "atlas-scripts-sml/MatrixAT.sml";

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

  fun select_columns (cols: int list, m: mat) : mat =
    let
      val (n, k) = IntMatrix.matShape m
      val () = if List.all (fn j => 0 <= j andalso j < k) cols then () else raise Fail "LatticeAT.select_columns: oob"
      val mcols = matColumns m
      val picked = List.map (fn j => List.nth (mcols, j)) cols
    in
      matFromColumnsN (n, picked)
    end

  fun select_rows (rows: int list, m: mat) : mat =
    IntMatrix.transpose (select_columns (rows, IntMatrix.transpose m))

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

  fun matInverseUnimodular (a: mat) : mat =
    let
      val (n, m) = IntMatrix.matShape a
      val () = if n = m then () else raise Fail "LatticeAT.matInverseUnimodular: non-square"
      val id = IntMatrix.identity n
    in
      case solve_mat (a, id) of
        NONE => raise Fail "LatticeAT.matInverseUnimodular: not invertible over Z"
      | SOME inv => inv
    end

  fun adapted_direct_sum (m: mat) : int * mat =
    let
      val (r, diag) = MatReduc.adaptedBasis m
    in
      (length diag, r)
    end

  fun image_subspace (m: mat) : mat =
    let
      val (c, r) = adapted_direct_sum m
    in
      select_columns (List.tabulate (c, fn i => i), r)
    end

  fun image_complement_basis (m: mat) : mat =
    let
      val (c, r) = adapted_direct_sum m
      val (n, _) = IntMatrix.matShape r
    in
      select_columns (List.tabulate (n - c, fn i => c + i), r)
    end

  val saturation = image_subspace
  val saturation_quotient_basis = image_complement_basis

  fun projector_to_image (m: mat) : mat =
    let
      val (c, r) = adapted_direct_sum m
      val (n, _) = IntMatrix.matShape r
      val invR = matInverseUnimodular r
      val cols = select_columns (List.tabulate (c, fn i => i), r) (* n x c *)
      val rows = select_rows (List.tabulate (c, fn i => i), invR) (* c x n *)
    in
      IntMatrix.matMul (cols, rows)
    end

  fun image_projector (m: mat) : mat =
    let
      val (c, r) = adapted_direct_sum m
      val invR = matInverseUnimodular r
    in
      select_rows (List.tabulate (c, fn i => i), invR)
    end

  fun projector_mod_image (m: mat) : mat =
    let
      val (c, r) = adapted_direct_sum m
      val (n, _) = IntMatrix.matShape r
      val invR = matInverseUnimodular r
      val cols = select_columns (List.tabulate (n - c, fn i => c + i), r) (* n x (n-c) *)
      val rows = select_rows (List.tabulate (n - c, fn i => c + i), invR) (* (n-c) x n *)
    in
      IntMatrix.matMul (cols, rows)
    end

  fun mod_image_projector (m: mat) : mat =
    let
      val (c, r) = adapted_direct_sum m
      val (n, _) = IntMatrix.matShape r
      val invR = matInverseUnimodular r
    in
      select_rows (List.tabulate (n - c, fn i => c + i), invR)
    end

  fun decompose (m: mat, v: vec) : vec * vec =
    let
      val (c, r) = adapted_direct_sum m
      val (n, _) = IntMatrix.matShape r
      val () = if length v = n then () else raise Fail "LatticeAT.decompose: dim mismatch"
      val invR = matInverseUnimodular r
      val projIm = projector_to_image m
      val projComp = projector_mod_image m
    in
      (IntMatrix.matVecMul (projIm, v), IntMatrix.matVecMul (projComp, v))
    end

  fun quotient_matrix (a: mat, m: mat) : mat =
    let
      val (c, r) = adapted_direct_sum m
      val (n, _) = IntMatrix.matShape r
      val invR = matInverseUnimodular r
      val compCols = select_columns (List.tabulate (n - c, fn i => c + i), r) (* n x (n-c) *)
      val compRows = select_rows (List.tabulate (n - c, fn i => c + i), invR) (* (n-c) x n *)
    in
      IntMatrix.matMul (compRows, IntMatrix.matMul (a, compCols))
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

  fun is_saturated_image (m: mat) : bool =
    let
      val (_, ds) = IntMatrix.smithBasis m
    in
      List.all (fn d => d = 1) ds
    end

  (* Port of `free_quotient_lattice_basis(mat M)` from `atlas-scripts/lattice.at`. *)
  fun free_quotient_lattice_basis (m: mat) : mat =
    let
      val e = image_lattice m
    in
      if is_saturated_image e then image_complement_basis e
      else raise Fail "LatticeAT.free_quotient_lattice_basis: sublattice is not a direct factor"
    end

  (* Port of `free_quotient_lattice_basis(L,M)` from `atlas-scripts/lattice_aux.at`. *)
  fun free_quotient_lattice_basis_LM (l: mat, m: mat) : mat =
    let
      val l1 = image_lattice l
      val (j, _) = MatrixAT.weak_left_inverse l1
      val jm = IntMatrix.matMul (j, m)
      val q = free_quotient_lattice_basis jm
    in
      IntMatrix.matMul (l1, q)
    end

  (* Port of `saturation_quotient_basis(M,L)` from `atlas-scripts/lattice_aux.at`. *)
  fun saturation_quotient_basis_ML (m: mat, l: mat) : mat =
    let
      val q = saturation_quotient_basis l
      val p = IntMatrix.matMul (q, IntMatrix.transpose q)
      val m1 = image_lattice m
    in
      image_lattice (IntMatrix.matMul (p, m1))
    end

  fun inv_fact (a: mat) : int list =
    let
      val (_, ds) = IntMatrix.smithBasis a
    in
      ds
    end

  fun is_sublattice (l: mat, m: mat) : bool =
    let
      val (nrL, _) = IntMatrix.matShape l
      val (nrM, _) = IntMatrix.matShape m
      val () = if nrL = nrM then () else raise Fail "LatticeAT.is_sublattice: row mismatch"
      val colsL = matColumns l
      fun ok v =
        (case Lattice.solve (m, v) of
           NONE => false
         | SOME _ => true)
    in
      List.all ok colsL
    end

  fun is_lattice_equal (l: mat, m: mat) : bool =
    is_sublattice (l, m) andalso is_sublattice (m, l)

  fun is_saturated (a: mat, b: mat) : bool =
    let
      val () = if is_sublattice (b, a) then () else raise Fail "LatticeAT.is_saturated: not a sublattice"
      val a1 = image_lattice a
      val b1 = image_lattice b
      val (_, ca) = IntMatrix.matShape a1
      val n = IntMatrix.kernel (IntMatrix.hcat (a1, b1))
      val m = IntMatrix.firstRows (ca, n)
      val w = inv_fact m
    in
      List.all (fn d => d = 1) w
    end

  fun quotient (a: mat, b: mat) : int list =
    let
      val () = if is_sublattice (a, b) then () else raise Fail "LatticeAT.quotient: not a sublattice"
      val a1 = image_lattice a
      val b1 = image_lattice b
      val (_, ca) = IntMatrix.matShape a1
      val n = IntMatrix.kernel (IntMatrix.hcat (b1, a1))
      val m = IntMatrix.firstRows (ca, n)
    in
      inv_fact m
    end
end
