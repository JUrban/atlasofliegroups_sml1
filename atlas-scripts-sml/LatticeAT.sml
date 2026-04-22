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
end

