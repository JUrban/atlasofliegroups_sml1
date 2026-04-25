use "atlas-scripts-sml/BigRat.sml";
use "atlas-scripts-sml/Gaussian_elim.sml";
use "atlas-scripts-sml/RootDatum.sml";
use "atlas-scripts-sml/structure_constants.sml";

(*
  File: atlas-scripts-sml/bracket.sml

  Purpose
  - Partial SML translation of `atlas-scripts/bracket.at`.
  - Provides a concrete datatype for Lie algebra elements expressed in the
    Chevalley/Lusztig basis indexed by roots (plus a Cartan part), together
    with the Lie bracket computed from a precomputed structure-constant table.

  What is implemented
  - `LieAlgebraElement` representation:
      - root-vector part: coefficients on the root basis vectors `X_α`
      - Cartan part: coefficients on an `H` basis compatible with Atlas
        root/coroot coordinates.
  - Bracket decomposition:
      - `[X_root, Y_root]` via structure constants `N(α,β)`
      - torus term from `[X_α, X_{-α}]`
      - off-diagonal terms from `[H, X_α]`
  - Linear algebra helpers:
      - adjoint matrix `ad(X)` as a rational matrix over `BigRat`
      - solve `ad(X) * Y = Z` using `Gaussian_elim.full_solve`

  Not yet implemented
  - Nilpotent-orbit and Jacobson–Morozov triple search utilities from the
    `.at` script. Those require additional infrastructure and are left as TODO.

  Notes on representation
  - This module uses exact rationals `BigRat.t` throughout (rather than the
    smaller `Lattice.ratvec` format) to avoid overflow in bracket/solve code.
  - Matrices use the `Gaussian_elim` convention: column-major lists of columns.
*)

structure Bracket = struct
  type vec = int list
  type rootdatum = RootDatum.t
  type sctable = StructureConstants.table

  type rat = BigRat.t
  type ratvec = rat list
  type ratmat = ratvec list (* column-major, as in Gaussian_elim *)

  type lie = {t: sctable, root: ratvec, h: ratvec}

  fun fail where' msg = raise Fail ("Bracket." ^ where' ^ ": " ^ msg)

  fun rat0 () = BigRat.zero ()
  fun rat1 () = BigRat.one ()
  fun ratInt (n: int) : rat = BigRat.fromInt n

  fun isZeroRat (q: rat) : bool = BigRat.isZero q

  fun ratAdd (a: rat, b: rat) : rat = BigRat.add (a, b)
  fun ratSub (a: rat, b: rat) : rat = BigRat.sub (a, b)
  fun ratNeg (a: rat) : rat = BigRat.neg a
  fun ratMul (a: rat, b: rat) : rat = BigRat.mul (a, b)
  fun ratMulInt (a: rat, k: int) : rat = BigRat.mulInt (a, k)

  fun ratEq (a: rat, b: rat) : bool = BigRat.equal (a, b)

  fun ratvecZero (n: int) : ratvec = List.tabulate (n, fn _ => rat0 ())

  fun ratvecAdd (xs: ratvec, ys: ratvec) : ratvec =
    ListPair.mapEq ratAdd (xs, ys)

  fun ratvecScaleInt (k: int, xs: ratvec) : ratvec =
    List.map (fn a => ratMulInt (a, k)) xs

  fun ratvecScale (c: rat, xs: ratvec) : ratvec =
    List.map (fn a => ratMul (c, a)) xs

  fun ratvecAllZero (xs: ratvec) : bool = List.all isZeroRat xs

  fun dotRatInt (xs: ratvec, ys: vec) : rat =
    let
      fun loop ([], [], acc) = acc
        | loop (a :: as', b :: bs', acc) = loop (as', bs', ratAdd (acc, ratMulInt (a, b)))
        | loop _ = fail "dotRatInt" "length mismatch"
    in
      loop (xs, ys, rat0 ())
    end

  fun vecAdd (a: vec, b: vec) : vec = ListPair.mapEq (op +) (a, b)
  fun vecNeg (a: vec) : vec = List.map (fn x => ~x) a

  fun root_datum_of_table (t: sctable) : rootdatum = #root_datum t
  fun roots_of_table (t: sctable) : vec list = #roots t

  fun rank_of_table (t: sctable) : int = RootDatum.rank (root_datum_of_table t)
  fun num_roots_of_table (t: sctable) : int = length (roots_of_table t)

  fun dim_of_table (t: sctable) : int = num_roots_of_table t + rank_of_table t

  fun idxOf (xs: vec list, v: vec) : int =
    let
      fun loop ([], _) = ~1
        | loop (x :: rest, i) = if x = v then i else loop (rest, i + 1)
    in
      loop (xs, 0)
    end

  fun basisVec (n: int, i: int) : ratvec =
    if i < 0 orelse i >= n then fail "basisVec" "oob"
    else List.tabulate (n, fn j => if i = j then rat1 () else rat0 ())

  fun null (t: sctable) : lie =
    {t = t, root = ratvecZero (num_roots_of_table t), h = ratvecZero (rank_of_table t)}

  fun is_zero (x: lie) : bool = ratvecAllZero (#root x) andalso ratvecAllZero (#h x)

  fun add (x: lie, y: lie) : lie =
    {t = #t x, root = ratvecAdd (#root x, #root y), h = ratvecAdd (#h x, #h y)}

  fun scaleInt (k: int, x: lie) : lie =
    {t = #t x, root = ratvecScaleInt (k, #root x), h = ratvecScaleInt (k, #h x)}

  fun scale (c: rat, x: lie) : lie =
    {t = #t x, root = ratvecScale (c, #root x), h = ratvecScale (c, #h x)}

  fun coordinates (x: lie) : ratvec = #root x @ #h x

  fun lie_algebra_element (t: sctable, v: ratvec) : lie =
    let
      val nR = num_roots_of_table t
      val r = rank_of_table t
      val () = if length v = nR + r then () else fail "lie_algebra_element" "dimension mismatch"
      val rootPart = List.take (v, nR)
      val hPart = List.drop (v, nR)
    in
      {t = t, root = rootPart, h = hPart}
    end

  fun lie_algebra_element_semisimple (t: sctable, v: ratvec) : lie =
    let
      val nR = num_roots_of_table t
      val r = rank_of_table t
      val () = if length v = r then () else fail "lie_algebra_element_semisimple" "rank mismatch"
    in
      {t = t, root = ratvecZero nR, h = v}
    end

  fun lie_algebra_element_root_vectors (t: sctable, v: ratvec) : lie =
    let
      val nR = num_roots_of_table t
      val r = rank_of_table t
      val () = if length v = nR then () else fail "lie_algebra_element_root_vectors" "roots mismatch"
    in
      {t = t, root = v, h = ratvecZero r}
    end

  fun basis (t: sctable, i: int) : lie =
    lie_algebra_element (t, basisVec (dim_of_table t, i))

  (* Pretty-print (simple; intended for debugging). *)
  fun formatRat (q: rat) : string =
    let
      val q = BigRat.normalize q
    in
      if #den q = 1 then
        IntInf.toString (#num q)
      else
        IntInf.toString (#num q) ^ "/" ^ IntInf.toString (#den q)
    end

  fun format (x: lie) : string =
    let
      val rs = roots_of_table (#t x)
      fun showIntList xs = "[" ^ String.concatWith "," (List.map Int.toString xs) ^ "]"
      val rootTerms =
        List.concat
          (ListPair.mapEq
             (fn (c, a) =>
                if isZeroRat c then []
                else ["+" ^ formatRat c ^ "*X_" ^ showIntList a])
             (#root x, rs))
      fun showRatVec xs = "[" ^ String.concatWith "," (List.map formatRat xs) ^ "]"
      val hPart = if ratvecAllZero (#h x) then "" else " H=" ^ showRatVec (#h x)
      val rootStr = String.concat rootTerms
    in
      rootStr ^ hPart
    end

  (* Height parity for sign (-1)^height(alpha). *)
  fun heightParitySign (rd: rootdatum, alpha: vec) : int =
    let
      val coeffs = RootDatum.rootExpression rd alpha
      val ht = List.foldl (op +) 0 coeffs
    in
      if Int.abs ht mod 2 = 0 then 1 else ~1
    end

  fun bracket_root_term (x: lie, y: lie) : lie =
    let
      val t = #t x
      val roots = roots_of_table t
      val nR = length roots
      val () = if nR = length (#root x) andalso nR = length (#root y) then () else fail "bracket_root_term" "root size mismatch"

      val out = Array.array (nR, rat0 ())
      fun addAt (k: int, c: rat) : unit =
        Array.update (out, k, ratAdd (Array.sub (out, k), c))

      fun loopI i =
        let
          val xi = List.nth (#root x, i)
        in
          if isZeroRat xi then ()
          else
            let
              val alpha = List.nth (roots, i)
              fun loopJ j =
                let
                  val yj = List.nth (#root y, j)
                in
                  if isZeroRat yj then ()
                  else
                    let
                      val n = StructureConstants.get (t, i, j)
                    in
                      if n = 0 then ()
                      else
                        let
                          val beta = List.nth (roots, j)
                          val sum = vecAdd (alpha, beta)
                          val k = idxOf (roots, sum)
                        in
                          if k < 0 then ()
                          else
                            addAt (k, ratMulInt (ratMul (xi, yj), n))
                        end
                    end
                end
            in
              List.app loopJ (List.tabulate (nR, fn j => j))
            end
        end

      val () = List.app loopI (List.tabulate (nR, fn i => i))
      val rootOut = List.tabulate (nR, fn i => Array.sub (out, i))
      val hOut = ratvecZero (rank_of_table t)
    in
      {t = t, root = rootOut, h = hOut}
    end

  fun bracket_torus_term (x: lie, y: lie) : lie =
    let
      val t = #t x
      val rd = root_datum_of_table t
      val roots = roots_of_table t
      val coroots = RootDatum.corootsCols rd
      val nR = length roots
      val r = rank_of_table t
      val () = if length coroots = nR then () else fail "bracket_torus_term" "roots/coroots mismatch"
      val outH = Array.array (r, rat0 ())

      fun addH (c: rat, hv: vec) : unit =
        let
          val () = if length hv = r then () else fail "bracket_torus_term" "coroot length mismatch"
          fun step (a, i) =
            Array.update (outH, i, ratAdd (Array.sub (outH, i), ratMulInt (c, a)))
        in
          List.app step (ListPair.zipEq (hv, List.tabulate (r, fn i => i)))
        end

      fun loopI i =
        let
          val xi = List.nth (#root x, i)
        in
          if isZeroRat xi then ()
          else
            let
              val alpha = List.nth (roots, i)
              val j = idxOf (roots, vecNeg alpha)
            in
              if j < 0 then ()
              else
                let
                  val yj = List.nth (#root y, j)
                in
                  if isZeroRat yj then ()
                  else
                    let
                      val sgn = heightParitySign (rd, alpha)
                      val c = ratMulInt (ratMul (xi, yj), sgn)
                      val hv = List.nth (coroots, i)
                    in
                      addH (c, hv)
                    end
                end
            end
        end

      val () = List.app loopI (List.tabulate (nR, fn i => i))
      val hOut = List.tabulate (r, fn i => Array.sub (outH, i))
    in
      {t = t, root = ratvecZero nR, h = hOut}
    end

  fun bracket_off_diagonal_terms (x: lie, y: lie) : lie =
    let
      val t = #t x
      val roots = roots_of_table t
      val nR = length roots
      val () = if length (#root x) = nR andalso length (#root y) = nR then () else fail "bracket_off_diagonal_terms" "root size mismatch"
      val () =
        if length (#h x) = rank_of_table t andalso length (#h y) = rank_of_table t then ()
        else fail "bracket_off_diagonal_terms" "H size mismatch"

      val out = Array.array (nR, rat0 ())
      fun addAt (i: int, c: rat) : unit =
        Array.update (out, i, ratAdd (Array.sub (out, i), c))

      fun loopI i =
        let
          val alpha = List.nth (roots, i)
          val xi = List.nth (#root x, i)
          val yi = List.nth (#root y, i)
          val dy = dotRatInt (#h y, alpha)
          val dx = dotRatInt (#h x, alpha)
          val term = ratAdd (ratNeg (ratMul (xi, dy)), ratMul (yi, dx))
        in
          if isZeroRat term then () else addAt (i, term)
        end

      val () = List.app loopI (List.tabulate (nR, fn i => i))
      val rootOut = List.tabulate (nR, fn i => Array.sub (out, i))
      val hOut = ratvecZero (rank_of_table t)
    in
      {t = t, root = rootOut, h = hOut}
    end

  fun bracket (x: lie, y: lie) : lie =
    add (add (bracket_root_term (x, y), bracket_torus_term (x, y)), bracket_off_diagonal_terms (x, y))

  fun X_alpha_index (t: sctable, i: int) : lie =
    let
      val nR = num_roots_of_table t
      val r = rank_of_table t
      val () = if 0 <= i andalso i < nR then () else fail "X_alpha_index" "oob"
    in
      {t = t, root = basisVec (nR, i), h = ratvecZero r}
    end

  fun X_alpha (t: sctable, alpha: vec) : lie =
    let
      val roots = roots_of_table t
      val i = idxOf (roots, alpha)
    in
      if i < 0 then fail "X_alpha" "root not found" else X_alpha_index (t, i)
    end

  fun H (t: sctable, h: vec) : lie =
    let
      val nR = num_roots_of_table t
      val r = rank_of_table t
      val () = if length h = r then () else fail "H" "rank mismatch"
    in
      {t = t, root = ratvecZero nR, h = List.map ratInt h}
    end

  fun ad (x: lie) : ratmat =
    let
      val t = #t x
      val dim = dim_of_table t
      fun col i = coordinates (bracket (x, basis (t, i)))
    in
      List.tabulate (dim, col)
    end

  fun solve_ad (x: lie, z: lie) : ratvec option =
    let
      val a = ad x
      val b = coordinates z
    in
      case Gaussian_elim.full_solve (a, b) of
        NONE => NONE
      | SOME sol => SOME (#base_point sol)
    end

  fun two_eigenspace (rd: rootdatum, h: ratvec) : vec list =
    let
      val roots = RootDatum.rootsCols rd
      val two = ratInt 2
    in
      List.filter (fn alpha => ratEq (dotRatInt (h, alpha), two)) roots
    end

  fun two_eigenspace_table (t: sctable, h: ratvec) : lie list =
    List.map (fn alpha => X_alpha (t, alpha)) (two_eigenspace (root_datum_of_table t, h))

  fun TODO (_: string) : 'a = raise Fail "Bracket: not yet ported (JM_triple/nilpotent utilities)"
end
