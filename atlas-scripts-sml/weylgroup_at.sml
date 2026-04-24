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
  type word = int list

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

  (* Multiply a matrix on the left by the simple reflection matrix. *)
  fun left_reflect (rd: rootdatum, s: int, m: mat) : mat =
    IntMatrix.matMul (reflection_matrix_simple (rd, s), m)

  (* Multiply a matrix on the right by the simple reflection matrix. *)
  fun right_reflect (rd: rootdatum, m: mat, s: int) : mat =
    IntMatrix.matMul (m, reflection_matrix_simple (rd, s))

  (* Conjugate by the simple reflection: `r * m * r`. *)
  fun conjugate (rd: rootdatum, s: int, m: mat) : mat =
    left_reflect (rd, s, right_reflect (rd, m, s))

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

  (* Apply a Weyl word left-to-right as successive simple reflections acting on
     weight vectors in X^* (the convention used internally by `from_simple`). *)
  fun act_word_ltr (rd: rootdatum, w: word, v: vec) : vec =
    List.foldl (fn (s, acc) => reflect_simple (rd, s, acc)) v w

  (* Port of `.at`:
       from_simple (RootDatum rd, vec alpha) = (WeylElt, vec)

     Input: `alpha` is assumed to be a positive root (as a vector in X^* ).
     Output: `(w, beta)` where `beta` is a simple root and `w` is a word such
     that applying the simple reflections in `w` (left-to-right) transforms the
     input root into `beta`.

     Notes
     - The `.at` implementation returns a full `WeylElt`. In SML we return just
       the word `int list`; callers can obtain the corresponding action matrix
       if needed by multiplying reflection matrices. *)
  fun from_simple (rd: rootdatum, alpha0: vec) : word * vec =
    let
      val ssr = RootDatum.semisimpleRank rd

      fun lastDescentIndex (alpha: vec) : int =
        let
          fun loop i best =
            if i = ssr then best
            else
              let
                val av = simple_coroot (rd, i)
              in
                if RootDatum.dot (av, alpha) > 0 then loop (i + 1) i else loop (i + 1) best
              end
        in
          loop 0 (~1)
        end

      fun loop (alpha: vec, acc: word) : word * vec =
        let
          val i = lastDescentIndex alpha
          val () = if i >= 0 then () else fail "from_simple" "not a positive root"
          val alpha_i = simple_root (rd, i)
        in
          if alpha = alpha_i then
            (List.rev acc, alpha)
          else
            loop (reflect_simple (rd, i, alpha), i :: acc)
        end
    in
      loop (alpha0, [])
    end
end
