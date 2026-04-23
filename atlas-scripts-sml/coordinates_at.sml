use "atlas-scripts-sml/RootDatum.sml";
use "atlas-scripts-sml/ratmat.sml";
use "atlas-scripts-sml/Lattice.sml";

(*
  File: atlas-scripts-sml/coordinates_at.sml

  Purpose
  - SML translation of `atlas-scripts/coordinates.at`.
  - Implements change-of-basis matrices between the simple-root basis of a
    `RootDatum` and a user-supplied “preferred” coordinate system.

  Atlas correspondence
  - The `.at` script defines:
      C = change_basis(rd,my_roots)
      D = inverse_change_basis(rd,my_roots)
    where `my_roots` is a matrix whose columns are the simple roots expressed
    in the user’s coordinates.
  - The intended identities are:
      D * my_roots = simple_roots(rd)
      C * simple_roots(rd) = my_roots
    and `D*C = I` on the semisimple lattice when dimensions match.

  Representation notes
  - This port uses `RatMat.ratmat` (exact rationals) for all outputs to avoid
    overflow issues in integer numerators/denominators.
*)

structure CoordinatesAT = struct
  type ratvec = Lattice.ratvec
  type ratmat = RatMat.ratmat

  fun change_basis_ratmat (rd: RootDatum.t, my_roots: ratmat) : ratmat =
    let
      val s = RootDatum.simpleRootsMat rd
      val invS = RatMat.rational_inverse s
    in
      RatMat.mul (my_roots, invS)
    end

  fun change_basis_ratvecs (rd: RootDatum.t, my_roots_cols: ratvec list) : ratmat =
    let
      val cols = List.map RatMat.fromRatvec my_roots_cols
    in
      change_basis_ratmat (rd, cols)
    end

  fun change_basis_intmat (rd: RootDatum.t, my_roots: IntMatrix.mat) : ratmat =
    change_basis_ratmat (rd, RatMat.ofIntMat my_roots)

  fun integral_change_basis_ratmat (rd: RootDatum.t, my_roots: ratmat) : IntMatrix.mat =
    (case RatMat.toIntMat (change_basis_ratmat (rd, my_roots)) of
       SOME m => m
     | NONE => raise Fail "CoordinatesAT.integral_change_basis: result not integral")

  fun integral_change_basis_ratvecs (rd: RootDatum.t, my_roots_cols: ratvec list) : IntMatrix.mat =
    integral_change_basis_ratmat (rd, List.map RatMat.fromRatvec my_roots_cols)

  fun integral_change_basis_intmat (rd: RootDatum.t, my_roots: IntMatrix.mat) : IntMatrix.mat =
    integral_change_basis_ratmat (rd, RatMat.ofIntMat my_roots)

  (*
    `inverse_change_basis` from `coordinates.at`

    The `.at` script defines:
      inverse_change_basis(rd,A) = simple_roots(rd) * left_inverse(A)
    where `left_inverse(A) = ^[A(^A A)^{-1}]`.

    In our `RatMat` port, `RatMat.left_inverse(A) = (A^T A)^{-1} A^T`, so:
      D = simple_roots(rd) * RatMat.left_inverse(A)
  *)

  fun inverse_change_basis_ratmat (rd: RootDatum.t, my_roots: ratmat) : ratmat =
    let
      val s = RatMat.ofIntMat (RootDatum.simpleRootsMat rd)
      val li = RatMat.left_inverse my_roots
    in
      RatMat.mul (s, li)
    end

  fun inverse_change_basis_ratvecs (rd: RootDatum.t, my_roots_cols: ratvec list) : ratmat =
    inverse_change_basis_ratmat (rd, List.map RatMat.fromRatvec my_roots_cols)

  fun inverse_change_basis_intmat (rd: RootDatum.t, my_roots: IntMatrix.mat) : ratmat =
    inverse_change_basis_ratmat (rd, RatMat.ofIntMat my_roots)

  fun integral_inverse_change_basis_ratmat (rd: RootDatum.t, my_roots: ratmat) : IntMatrix.mat =
    (case RatMat.toIntMat (inverse_change_basis_ratmat (rd, my_roots)) of
       SOME m => m
     | NONE => raise Fail "CoordinatesAT.integral_inverse_change_basis: result not integral")

  fun integral_inverse_change_basis_ratvecs (rd: RootDatum.t, my_roots_cols: ratvec list) : IntMatrix.mat =
    integral_inverse_change_basis_ratmat (rd, List.map RatMat.fromRatvec my_roots_cols)

  fun integral_inverse_change_basis_intmat (rd: RootDatum.t, my_roots: IntMatrix.mat) : IntMatrix.mat =
    integral_inverse_change_basis_ratmat (rd, RatMat.ofIntMat my_roots)
end

