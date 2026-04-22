use "atlas-scripts-sml/LatticeAT.sml";
use "atlas-scripts-sml/Lattice.sml";

(* Direct SML analogue of `atlas-scripts/lattice_aux.at`, implemented on top of
   the `LatticeAT` compatibility layer. *)
structure LatticeAux = struct
  type vec = LatticeAT.vec
  type mat = LatticeAT.mat
  type ratvec = LatticeAT.ratvec

  val select_columns = LatticeAT.select_columns
  val select_rows = LatticeAT.select_rows

  fun first_columns (n: int, m: mat) : mat =
    select_columns (List.tabulate (n, fn i => i), m)

  fun first_rows (n: int, m: mat) : mat =
    select_rows (List.tabulate (n, fn i => i), m)

  val intersection_plus = LatticeAT.intersection_plus
  val intersection = LatticeAT.intersection

  fun intersection_list (ms: mat list) : mat =
    (case ms of
       [] => raise Fail "LatticeAux.intersection_list: empty"
     | m0 :: rest => List.foldl (fn (m, acc) => intersection (acc, m)) m0 rest)

  (* `lattice_aux.at` defines `free_quotient_lattice_basis(L,M)` and
     `saturation_quotient_basis(M,L)`, while `lattice.at` has 1-arg versions. *)
  fun free_quotient_lattice_basis (l: mat, m: mat) : mat =
    LatticeAT.free_quotient_lattice_basis_LM (l, m)

  fun saturation_quotient_basis (m: mat, l: mat) : mat =
    LatticeAT.saturation_quotient_basis_ML (m, l)

  val projector_mod_image = LatticeAT.projector_mod_image
  val mod_image_projector = LatticeAT.mod_image_projector

  val is_sublattice = LatticeAT.is_sublattice
  val is_lattice_equal = LatticeAT.is_lattice_equal

  fun is_subspace (l: mat, m: mat) : bool =
    let
      val colsL = LatticeAT.matColumns l
      fun ok v = Option.isSome (Lattice.vec_solve (m, {den = 1, nums = v}))
    in
      List.all ok colsL
    end

  val is_saturated = LatticeAT.is_saturated
  val quotient = LatticeAT.quotient

  val image_lattice = LatticeAT.image_lattice
  val image_lattice_plus = LatticeAT.image_lattice_plus
end

