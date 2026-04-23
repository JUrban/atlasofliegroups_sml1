use "atlas-scripts-sml/IntMatrix.sml";
use "atlas-scripts-sml/RootDatum.sml";

(*
  File: atlas-scripts-sml/weylgroup_at.sml

  Purpose
  - Small SML port of a few low-level Weyl-group utilities from:
      - `atlas-scripts/basic.at` (reflection/coreflection, matrix conjugation)
      - `atlas-scripts/Weylgroup.at` (the `lengthens` predicates)
  - This file exists to support future `.at`→`.sml` ports that expect these
    building blocks (notably `tits.at`), without pulling in the full Atlas
    interpreter.

  Representation
  - Vectors are `int list` of length `rank(rd)` (weights/coweights in Atlas
    coordinates).
  - Matrices are row-major `IntMatrix.mat`.

  Important convention
  - In `.at`, `root(rd,s)` and `coroot(rd,s)` for a simple generator `s`
    refer to the *simple* root/coroot. We implement simple-reflection
    operations using `RootDatum.simpleRootsCols` / `simpleCorootsCols`.
*)

structure WeylgroupAT = struct
  type rootdatum = RootDatum.t
  type vec = int list
  type mat = IntMatrix.mat

  fun fail where' msg = raise Fail ("WeylgroupAT." ^ where' ^ ": " ^ msg)

  fun simple_root (rd: rootdatum, s: int) : vec =
    List.nth (RootDatum.simpleRootsCols rd, s)

  fun simple_coroot (rd: rootdatum, s: int) : vec =
    List.nth (RootDatum.simpleCorootsCols rd, s)

  fun vecAdd (xs: vec, ys: vec) : vec =
    ListPair.mapEq (op +) (xs, ys)

  fun vecSub (xs: vec, ys: vec) : vec =
    ListPair.mapEq (op -) (xs, ys)

  fun vecScale (k: int, xs: vec) : vec = List.map (fn a => k * a) xs

  (* Outer product `a * b^T` for column vectors `a` and `b`. *)
  fun outerProduct (a: vec, b: vec) : mat =
    let
      val n = length a
      val () = if length b = n then () else fail "outerProduct" "length mismatch"
      fun row i =
        let
          val ai = List.nth (a, i)
        in
          List.map (fn bj => ai * bj) b
        end
    in
      List.tabulate (n, row)
    end

  (* Matrix for the simple reflection `s` acting on X^* (weights): `I - alpha*alpha^v^T`. *)
  fun reflection_matrix_simple (rd: rootdatum, s: int) : mat =
    let
      val alpha = simple_root (rd, s)
      val alphav = simple_coroot (rd, s)
      val n = length alpha
      val () = if length alphav = n then () else fail "reflection_matrix_simple" "dim mismatch"
      val id = IntMatrix.identity n
      val opm = outerProduct (alpha, alphav)
    in
      IntMatrix.sub (id, opm)
    end

  (* Reflect a weight vector: `v <- v - <v,alpha^v>*alpha`. *)
  fun reflect_simple (rd: rootdatum, s: int, v: vec) : vec =
    let
      val alpha = simple_root (rd, s)
      val alphav = simple_coroot (rd, s)
      val m = RootDatum.dot (v, alphav)
    in
      vecSub (v, vecScale (m, alpha))
    end

  (* Coreflect a coweight row vector: `v <- v - <alpha,v>*alpha^v`. *)
  fun coreflect_simple (rd: rootdatum, v: vec, s: int) : vec =
    let
      val alpha = simple_root (rd, s)
      val alphav = simple_coroot (rd, s)
      val m = RootDatum.dot (alpha, v)
    in
      vecSub (v, vecScale (m, alphav))
    end

  (* Multiply a row vector by a matrix: `v*M`. *)
  fun rowVecMul (v: vec, m: mat) : vec =
    let
      val (nRows, nCols) = IntMatrix.matShape m
      val () = if length v = nRows then () else fail "rowVecMul" "dim mismatch"
      fun col j = List.map (fn row => List.nth (row, j)) m
      val cols = List.tabulate (nCols, col)
    in
      List.map (fn c => RootDatum.dot (v, c)) cols
    end

  (* Whether a root vector is positive in the Atlas root-index convention. *)
  fun is_positive_root (rd: rootdatum, alpha: vec) : bool =
    let
      val npr = RootDatum.numPosRoots rd
      val i = RootDatum.rootIndex (rd, alpha)
    in
      if i = npr then fail "is_positive_root" "vector is not a root" else i >= 0
    end

  fun is_positive_coroot (rd: rootdatum, alphav: vec) : bool =
    let
      val npr = length (RootDatum.posCorootsCols rd)
      val i = RootDatum.corootIndex (rd, alphav)
    in
      if i = npr then fail "is_positive_coroot" "vector is not a coroot" else i >= 0
    end

  (*
    `.at`: lengthens(rd, M, s) for right multiplication by generator `s`.

    In `Weylgroup.at`:
      lengthens(rd,M,s) = rd.is_positive_root(M*root(rd,s))
  *)
  fun lengthens (rd: rootdatum, m: mat, s: int) : bool =
    is_positive_root (rd, IntMatrix.matVecMul (m, simple_root (rd, s)))

  (*
    `.at`: lengthens(rd, s, M) for left multiplication.

    In `Weylgroup.at`:
      lengthens(rd,s,M) = rd.is_positive_coroot(coroot(rd,s)*M)
  *)
  fun lengthens_left (rd: rootdatum, s: int, m: mat) : bool =
    is_positive_coroot (rd, rowVecMul (simple_coroot (rd, s), m))
end

