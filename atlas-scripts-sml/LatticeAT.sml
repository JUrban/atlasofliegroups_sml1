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

  fun solve_mat (a: mat, b: mat) : mat option =
    let
      val (na, _) = IntMatrix.matShape a
      val (nb, mb) = IntMatrix.matShape b
      val () = if na = nb then () else raise Fail "LatticeAT.solve_mat: row mismatch"
      val bcols = matColumns b
      fun loop ([], acc) = SOME (matFromColumns (List.rev acc))
        | loop (v :: vs, acc) =
            (case Lattice.solve (a, v) of
               NONE => NONE
             | SOME x => loop (vs, x :: acc))
    in
      loop (bcols, [])
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
end
