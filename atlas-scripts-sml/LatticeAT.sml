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
