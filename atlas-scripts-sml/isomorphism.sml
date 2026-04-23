use "atlas-scripts-sml/basic.sml";
use "atlas-scripts-sml/MatrixAT.sml";
use "atlas-scripts-sml/IntMatrix.sml";
use "atlas-scripts-sml/RootDatum.sml";
use "atlas-scripts-sml/ratmat.sml";
use "atlas-scripts-sml/Gaussian_elim.sml";
use "atlas-scripts-sml/BigRat.sml";
use "atlas-scripts-sml/lietypes.sml";

(*
  File: atlas-scripts-sml/isomorphism.sml

  Purpose
  - SML translation of `atlas-scripts/isomorphism.at`.
  - Computes diagram permutations and (local) isomorphisms between (semi)simple
    root data by comparing Cartan matrices and solving for the change-of-basis
    matrix `g` satisfying `g * R1 = R2 * P`, where `Rk` is the matrix of simple
    roots of `rd_k` and `P` is a permutation matrix.

  Key correspondence notes
  - The `.at` file enumerates permutation matrices using `W(GL(n))`. In this
    SML port we enumerate *all* permutations of `0..n-1` directly (factorial
    growth; intended only for small ranks, as in the `.at` script).
  - `is_locally_isomorphic` is provided by `atlas-scripts-sml/lietypes.sml`
    (matching the `.at` logic: compare canonical Lie types).

  Matrix conventions
  - `IntMatrix.mat` is row-major.
  - `RootDatum.simpleRootsMat` is an `IntMatrix.mat` whose columns are the
    simple roots in the ambient lattice basis.
  - `RatMat.ratmat` is column-major (see `atlas-scripts-sml/ratmat.sml`).
*)

structure Isomorphism = struct
  type rootdatum = RootDatum.t
  type mat = IntMatrix.mat
  type ratmat = RatMat.ratmat

  fun isSemisimple (rd: rootdatum) : bool =
    RootDatum.rank rd = RootDatum.semisimpleRank rd

  (* List permutations of a list (factorial growth; for small n only). *)
  fun permutations (xs: 'a list) : 'a list list =
    let
      fun ins (x, []) = [[x]]
        | ins (x, y :: ys) =
            (x :: y :: ys) :: List.map (fn zs => y :: zs) (ins (x, ys))
      fun perms [] = [[]]
        | perms (x :: xs) = List.concat (List.map (fn p => ins (x, p)) (perms xs))
    in
      perms xs
    end

  fun permutation_matrices (n: int) : mat list =
    List.map MatrixAT.permutation_matrix (permutations (List.tabulate (n, fn i => i)))

  fun detIsPlusMinus1 (m: mat) : bool =
    let
      val d = Gaussian_elim.det (RatMat.ofIntMat m)
      val one = BigRat.one ()
    in
      BigRat.equal (d, one) orelse BigRat.equal (d, BigRat.neg one)
    end

  (*
    `.at`: root_permutations(rd1, rd2) : [mat]

    Return all permutation matrices `P` of size `n = semisimple_rank(rd1)`
    satisfying `P*C1 = C2*P`, where `Ck` is the Cartan matrix of `rd_k`.
  *)
  fun root_permutations (rd1: rootdatum, rd2: rootdatum) : mat list =
    if not (LieTypes.is_locally_isomorphic (rd1, rd2)) then
      []
    else
      let
        val n = RootDatum.semisimpleRank rd1
      in
        if n = 0 then
          [IntMatrix.identity 0]
        else
          let
            val c1 = RootDatum.cartanMatrix rd1
            val c2 = RootDatum.cartanMatrix rd2
            fun ok p = IntMatrix.matMul (p, c1) = IntMatrix.matMul (c2, p)
          in
            List.filter ok (permutation_matrices n)
          end
      end

  (*
    `.at`: root_permutation(rd1, rd2) : (bool, mat)

    Return whether a matching permutation exists, and one witness matrix.
  *)
  fun root_permutation (rd1: rootdatum, rd2: rootdatum) : bool * mat =
    if not (LieTypes.is_locally_isomorphic (rd1, rd2)) then
      (false, IntMatrix.identity 0)
    else if RootDatum.semisimpleRank rd1 = 0 then
      (true, IntMatrix.identity 0)
    else
      let
        val c1 = RootDatum.cartanMatrix rd1
        val c2 = RootDatum.cartanMatrix rd2
        val n = RootDatum.semisimpleRank rd1
        fun ok p = IntMatrix.matMul (p, c1) = IntMatrix.matMul (c2, p)
      in
        case List.find ok (permutation_matrices n) of
          NONE => (false, [])
        | SOME p => (true, p)
      end

  (*
    `.at`: local_isomorphism_long(rd1, rd2) : (bool, ratmat, mat)

    Semisimple case only. If `P` is a root permutation, solve for
      g = R2 * P * inverse(R1)
    (over Q), so that `g * R1 = R2 * P`.
  *)
  fun local_isomorphism_long (rd1: rootdatum, rd2: rootdatum) : bool * ratmat * mat =
    let
      val () =
        if isSemisimple rd1 andalso isSemisimple rd2 then ()
        else raise Fail "Isomorphism.local_isomorphism_long: root data must be semisimple"
      val (valid, p) = root_permutation (rd1, rd2)
    in
      if not valid then
        (false, RatMat.ofIntMat (IntMatrix.identity 0), IntMatrix.identity 0)
      else
        let
          val r1 = RatMat.ofIntMat (RootDatum.simpleRootsMat rd1)
          val r2 = RatMat.ofIntMat (RootDatum.simpleRootsMat rd2)
          val pr = RatMat.ofIntMat p
          val r1inv = RatMat.inverse r1
          val g = RatMat.mul (RatMat.mul (r2, pr), r1inv)
        in
          (true, g, p)
        end
    end

  (* `.at`: local_isomorphism(rd1, rd2) : ratmat (raises on failure). *)
  fun local_isomorphism (rd1: rootdatum, rd2: rootdatum) : ratmat =
    let
      val (valid, m, _) = local_isomorphism_long (rd1, rd2)
    in
      if valid then m else raise Fail "Isomorphism.local_isomorphism: root data are not locally isomorphic"
    end

  (*
    `.at`: isomorphism_long(rd1, rd2) : (bool, mat, mat)

    Try all root permutations `P` and return one for which the corresponding
    `g = R2 * P * inverse(R1)` is integral and unimodular (det = ±1).
  *)
  fun isomorphism_long (rd1: rootdatum, rd2: rootdatum) : bool * mat * mat =
    let
      val () =
        if isSemisimple rd1 andalso isSemisimple rd2 then ()
        else raise Fail "Isomorphism.isomorphism_long: root data must be semisimple"

      val perms = root_permutations (rd1, rd2)
      val r1 = RatMat.ofIntMat (RootDatum.simpleRootsMat rd1)
      val r2 = RatMat.ofIntMat (RootDatum.simpleRootsMat rd2)
      val r1inv = RatMat.inverse r1

      fun tryP [] = (false, IntMatrix.identity 0, IntMatrix.identity 0)
        | tryP (p :: ps) =
            let
              val gRat = RatMat.mul (RatMat.mul (r2, RatMat.ofIntMat p), r1inv)
            in
              case RatMat.toIntMat gRat of
                NONE => tryP ps
              | SOME gInt =>
                  if detIsPlusMinus1 gInt then (true, gInt, p) else tryP ps
            end
    in
      tryP perms
    end

  (* `.at`: isomorphism(rd1, rd2) : mat (raises on failure). *)
  fun isomorphism (rd1: rootdatum, rd2: rootdatum) : mat =
    let
      val (valid, m, _) = isomorphism_long (rd1, rd2)
    in
      if valid then m else raise Fail "Isomorphism.isomorphism: root data are not isomorphic"
    end

  (*
    `.at`: is_isomorphic(rd1, rd2) : bool

    Semisimple case: definitive.
    Reductive case: return false if not locally isomorphic; otherwise raise.
  *)
  fun is_isomorphic (rd1: rootdatum, rd2: rootdatum) : bool =
    if not (isSemisimple rd1 andalso isSemisimple rd2) then
      if not (LieTypes.is_locally_isomorphic (rd1, rd2)) then
        false
      else
        raise Fail "Isomorphism.is_isomorphic: root data are not semisimple (and are locally isomorphic)"
    else
      #1 (isomorphism_long (rd1, rd2))
end

