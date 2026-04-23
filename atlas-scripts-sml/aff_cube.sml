use "atlas-scripts-sml/BigRat.sml";
use "atlas-scripts-sml/Gaussian_elim.sml";
use "atlas-scripts-sml/combinatorics.sml";
use "atlas-scripts-sml/misc.sml";

(*
  File: atlas-scripts-sml/aff_cube.sml

  Purpose
  - Partial SML translation of `atlas-scripts/aff_cube.at`.
  - Implements the self-contained “affine subspace ∩ unit cube extrema”
    routines:
      - enumerate cube faces up to a rank bound
      - restrict a linear system to a face
      - find “good interior points” (unique intersection points in a face’s
        interior)
      - enumerate candidate extreme points for `H ∩ [0,1]^n`

  Representation (matching `.at` conventions)
  - Integer matrices are column-major: `type mat = int list list` where each
    column is a length-`m` list of row entries.
  - Integer vectors are `int list`.
  - Rational vectors are lists of `BigRat.t` scalars (no common-denominator
    packing here; we rely on `BigRat` + `Gaussian_elim`).

  Scope / limitations
  - This file ports only the cube/linear-algebra core. The later Lie-theoretic
    functions in `aff_cube.at` (involving Cartan classes, FPP data, etc.) are
    not included yet.
*)

structure Aff_cube = struct
  type vec = int list
  type mat = int list list (* column-major *)

  type rat = BigRat.t
  type ratvec = rat list

  fun rat0 () = BigRat.zero ()
  fun rat1 () = BigRat.one ()

  fun ratOfInt n = BigRat.fromInt n

  fun ratNormalize q = BigRat.normalize q

  fun ratLeq (a: rat, b: rat) : bool =
    let
      val a = ratNormalize a
      val b = ratNormalize b
    in
      #num a * #den b <= #num b * #den a
    end

  fun ratLt (a: rat, b: rat) : bool = ratLeq (a, b) andalso not (ratLeq (b, a))

  fun matShapeCols (a: mat) : int * int =
    (case a of
       [] => (0, 0)
     | col0 :: cols =>
         let
           val m = length col0
           val () = if List.all (fn c => length c = m) cols then () else raise Fail "Aff_cube: ragged matrix"
         in
           (m, length a)
         end)

  fun vecAdd (a: vec, b: vec) : vec = ListPair.mapEq (op +) (a, b)
  fun vecSub (a: vec, b: vec) : vec = ListPair.mapEq (op -) (a, b)
  fun vecScale (c: int, v: vec) : vec = List.map (fn x => c * x) v

  fun ratVecAdd (a: ratvec, b: ratvec) : ratvec = ListPair.mapEq BigRat.add (a, b)
  fun ratVecScale (c: rat, v: ratvec) : ratvec = List.map (fn x => BigRat.mul (c, x)) v

  fun matVecMulRat (a: mat, x: ratvec) : ratvec =
    let
      val (m, n) = matShapeCols a
      val () = if length x = n then () else raise Fail "Aff_cube.matVecMulRat: dim mismatch"
      val res = Array.array (m, rat0 ())
      fun addCol (col: vec, coeff: rat) =
        let
          fun loop ([], _, _) = ()
            | loop (v :: vs, i, r) =
                (Array.update (res, i, BigRat.add (Array.sub (res, i), BigRat.mul (coeff, ratOfInt v)));
                 loop (vs, i + 1, r))
        in
          loop (col, 0, ())
        end
      val () = List.app addCol (ListPair.zipEq (a, x))
    in
      Array.foldr (op ::) [] res
    end

  fun ratVecEqInt (v: ratvec, b: vec) : bool =
    let
      val () = if length v = length b then () else raise Fail "Aff_cube.ratVecEqInt: dim mismatch"
      fun eq (q, i) = BigRat.equal (q, ratOfInt i)
    in
      ListPair.allEq eq (v, b)
    end

  (* Rank over Q of an integer matrix given by columns. *)
  fun rankQ (a: mat) : int =
    (case a of
       [] => 0
     | _ =>
         let
           val cols : Gaussian_elim.mat = List.map (fn col => List.map ratOfInt col) a
           val (pivots, _, _) = Gaussian_elim.split_data (Gaussian_elim.get_data cols)
         in
           length pivots
         end)

  (* Solve A*x=b over Q (column-major A), returning a particular solution if consistent. *)
  fun solveQ (a: mat, b: vec) : ratvec option =
    (case a of
       [] =>
         if List.all (fn x => x = 0) b then SOME [] else NONE
     | col0 :: cols =>
         let
           val m = length col0
           val () = if List.all (fn c => length c = m) cols then () else raise Fail "Aff_cube.solveQ: ragged"
           val () = if length b = m then () else raise Fail "Aff_cube.solveQ: equation mismatch"
           val aRat : Gaussian_elim.mat = List.map (fn col => List.map ratOfInt col) a
           val bRat : Gaussian_elim.vec = List.map ratOfInt b
         in
           (case Gaussian_elim.full_solve (aRat, bRat) of
              NONE => NONE
            | SOME sol => SOME (#base_point sol))
         end)

  (* GOOD EXTREME POINT of F∩H on a given face (core from `.at`). *)
  fun good_interior_point (a: mat, b: vec) : ratvec list =
    let
      val (_, n) = matShapeCols a
    in
      if rankQ a < n then
        []
      else
        (case solveQ (a, b) of
           NONE => []
         | SOME x =>
             let
               val ok = List.all (fn xi => ratLt (rat0 (), xi) andalso ratLt (xi, rat1 ())) x
             in
               if ok then [x] else []
             end)
    end

  (* Complement of S in [0..n-1], assuming S has distinct entries in range. *)
  fun complement (n: int, s: int list) : int list =
    let
      val flags = Array.array (n, false)
      val () = List.app (fn i => if 0 <= i andalso i < n then Array.update (flags, i, true) else raise Fail "Aff_cube.complement: oob") s
    in
      List.filter (fn i => not (Array.sub (flags, i))) (List.tabulate (n, fn i => i))
    end

  (* Faces of dim at most r; returns pairs (S,epsilon) as in `.at`. *)
  fun cube_faces (n: int, r: int) : (int list * vec) list =
    let
      val () = if n < 0 orelse r < 0 then raise Fail "Aff_cube.cube_faces: negative" else ()
      val r = Int.min (r, n)
      fun loopM m =
        if m > n then []
        else
          let
            val count = Combinatorics.binom (n, m)
            val decode = Combinatorics.combination_decode m
            val betas = Misc.box (2, m) (* 0/1 vectors of length m *)
            fun oneJ j =
              let
                val s = decode j
              in
                List.map (fn beta => (s, beta)) betas
              end
            val ss = List.concat (List.tabulate (count, oneJ))
          in
            ss @ loopM (m + 1)
          end
    in
      loopM (n - r)
    end

  (* All faces of the n-cube (no rank bound), as in `cube_faces(n)` in `.at`. *)
  fun cube_faces_all (n: int) : (int list * vec) list =
    let
      val ss = Basic.power_set_int n
      fun oneS s =
        let
          val m = length s
          val betas = Misc.box (2, m)
        in
          List.map (fn beta => (s, beta)) betas
        end
    in
      List.concat (List.map oneS ss)
    end

  (* Restrict Av=b to the face given by fixing v[i]=epsilon[pos] for i in S. *)
  fun face_restrict (a: mat, b: vec, s: int list, epsilon: vec) : mat * vec =
    let
      val (m, n) = matShapeCols a
      val () = if length b = m then () else raise Fail "Aff_cube.face_restrict: b mismatch"
      val () = if length s = length epsilon then () else raise Fail "Aff_cube.face_restrict: epsilon mismatch"
      val t = complement (n, s)
      val ares = List.map (fn j => List.nth (a, j)) t
      val shift =
        List.foldl
          (fn ((i, eps), acc) => vecAdd (acc, vecScale (eps, List.nth (a, i))))
          (List.tabulate (m, fn _ => 0))
          (ListPair.zipEq (s, epsilon))
      val bres = vecSub (b, shift)
    in
      (ares, bres)
    end

  (* Enumerate candidate “good extreme points” for H∩[0,1]^n. *)
  fun aff_cube_extrema (a: mat, b: vec) : (ratvec * int list * vec) list =
    let
      val (_, n) = matShapeCols a
      val r = rankQ a
      val faces = cube_faces (n, r)
      fun buildFull (t: int list, s: int list, eps: vec, x: ratvec) : ratvec =
        let
          val v = Array.array (n, rat0 ())
          val () = List.app (fn (k, m) => Array.update (v, k, List.nth (x, m))) (ListPair.zipEq (t, List.tabulate (length t, fn i => i)))
          val () = List.app (fn (j, e) => Array.update (v, j, ratOfInt e)) (ListPair.zipEq (s, eps))
        in
          Array.foldr (op ::) [] v
        end

      fun oneFace (s, eps) =
        let
          val t = complement (n, s)
          val (ares, bres) = face_restrict (a, b, s, eps)
          val sols = good_interior_point (ares, bres)
        in
          case sols of
            [x] =>
              let
                val v = buildFull (t, s, eps, x)
              in
                if ratVecEqInt (matVecMulRat (a, v), b) then [(v, s, eps)] else []
              end
          | _ => []
        end
    in
      List.concat (List.map oneFace faces)
    end

  (* Short-circuit check: return true if there is at least one “good interior point”
     on some face up to rank(A), as in `.at` `aff_cube_extrema_short`. *)
  fun aff_cube_extrema_short (a: mat, b: vec) : bool =
    let
      val (_, n) = matShapeCols a
      val r = rankQ a
      val faces = cube_faces (n, r)
      fun ok (s, eps) =
        let
          val (ares, bres) = face_restrict (a, b, s, eps)
        in
          (case good_interior_point (ares, bres) of
             [_] => true
           | _ => false)
        end
    in
      List.exists ok faces
    end
end
