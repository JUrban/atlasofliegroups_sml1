use "atlas-scripts-sml/basic.sml";
use "atlas-scripts-sml/Lattice.sml";
use "atlas-scripts-sml/RootDatum.sml";
use "atlas-scripts-sml/Gaussian_elim.sml";

(*
  File: atlas-scripts-sml/chopping_facets.sml

  Purpose
  - SML translation of `atlas-scripts/chopping_facets.at`.
  - Implements convex-hull utilities and a “chop by coroot hyperplanes”
    routine used in Atlas scripts that subdivide a convex region by the affine
    coroot arrangement.

  High-level description (matching the `.at` intent)
  - Input is a finite vertex set `V ⊂ X^*(T) ⊗_Z Q` (represented as `ratvec`s).
  - For an integral functional `f` (typically a coroot), the region is chopped
    along the integer translates of the hyperplanes `f = n`:
      - first, new vertices are inserted on edges so that `f` takes integral
        values at those inserted points;
      - then, the vertex set is partitioned into “slabs” and “slices” defined
        by `n <= f <= n+1` and `f = n`;
      - finally, each piece is reduced to its convex-hull vertex set.
  - Iterating this over all positive coroots yields a subdivision of the
    original convex region into well-defined minimal vertex sets.

  Types and representation
  - `ratvec` is `Lattice.ratvec = {den:int, nums:int list}`, i.e. a rational
    vector with a common denominator (int-based; may overflow for huge data).
  - Rational scalars in this file use `BigRat.t` (exact, IntInf-backed).
  - Matrices for Gaussian elimination are lists of columns, as in
    `Gaussian_elim` / `Gaussian_elim.at`.

  Notes / limitations
  - This is a direct port of the core geometry routines; the (unrelated) Levi
    helper at the bottom of the `.at` file depends on `Levi_subgroups` and is
    left as a stub here.
  - The algorithms are correct but not optimized; they are intended primarily
    to make translated scripts self-contained and compilable under Poly/ML.
*)

structure Chopping_facets = struct
  type ratvec = Lattice.ratvec
  type functional = int list

  type rat = BigRat.t

  (* ---------- small helpers ---------- *)

  val maxInt = Option.valOf Int.maxInt
  val minInt = Option.valOf Int.minInt

  fun intOfIntInf (x: IntInf.int) : int =
    if x > IntInf.fromInt maxInt orelse x < IntInf.fromInt minInt then
      raise Fail "Chopping_facets: IntInf out of int range"
    else
      IntInf.toInt x

  fun normalizeRat (q: rat) : rat = BigRat.normalize q

  fun ratLeq (a: rat, b: rat) : bool =
    let
      val a = normalizeRat a
      val b = normalizeRat b
    in
      #num a * #den b <= #num b * #den a
    end

  fun ratLt (a: rat, b: rat) : bool = ratLeq (a, b) andalso not (ratLeq (b, a))
  fun ratGt (a: rat, b: rat) : bool = ratLt (b, a)

  fun ratMin (xs: rat list) : rat =
    (case xs of
       [] => raise Fail "Chopping_facets.ratMin: empty"
     | x :: rest => List.foldl (fn (y, acc) => if ratLeq (y, acc) then y else acc) x rest)

  fun ratMax (xs: rat list) : rat =
    (case xs of
       [] => raise Fail "Chopping_facets.ratMax: empty"
     | x :: rest => List.foldl (fn (y, acc) => if ratLeq (acc, y) then y else acc) x rest)

  fun ratIsInteger (q: rat) : bool = (#den (normalizeRat q) = 1)

  fun ratFloorIntInf (q: rat) : IntInf.int =
    let
      val q = normalizeRat q
      val n = #num q
      val d = #den q
    in
      IntInf.div (n, d)
    end

  fun ratCeilIntInf (q: rat) : IntInf.int =
    let
      val q = normalizeRat q
      val n = #num q
      val d = #den q
      val r = IntInf.mod (n, d)
    in
      if r = 0 then IntInf.div (n, d) else IntInf.div (n, d) + 1
    end

  fun ratFloor (q: rat) : int = intOfIntInf (ratFloorIntInf q)
  fun ratCeil (q: rat) : int = intOfIntInf (ratCeilIntInf q)

  fun dotIntInf (xs: int list, ys: int list) : IntInf.int =
    let
      fun loop ([], [], acc) = acc
        | loop (a :: as', b :: bs', acc) = loop (as', bs', acc + IntInf.fromInt a * IntInf.fromInt b)
        | loop _ = raise Fail "Chopping_facets.dot: length mismatch"
    in
      loop (xs, ys, 0)
    end

  fun deleteAt (xs: 'a list, idx: int) : 'a list =
    let
      fun loop ([], _, acc) = List.rev acc
        | loop (x :: rest, i, acc) = if i = idx then List.rev acc @ rest else loop (rest, i + 1, x :: acc)
    in
      if idx < 0 orelse idx >= length xs then raise Subscript else loop (xs, 0, [])
    end

  (* `ratvec` operations. *)
  fun ratvecNeg (v: ratvec) : ratvec = Lattice.ratvecNormalize {den = #den v, nums = List.map (fn x => ~x) (#nums v)}
  fun ratvecAdd (a: ratvec, b: ratvec) : ratvec = Lattice.ratvecSub (a, ratvecNeg b)

  fun ratvecMid (a: ratvec, b: ratvec) : ratvec =
    Lattice.ratvecScale (ratvecAdd (a, b), 1, 2)

  (* Convert a `ratvec` into a list of scalar rationals (one per coordinate). *)
  fun to_rat (v: ratvec) : rat list =
    let
      val d = IntInf.fromInt (#den v)
    in
      List.map (fn n => BigRat.make (IntInf.fromInt n, d)) (#nums v)
    end

  (* Convert a list of scalar rationals into a `ratvec` with common denom.
     Fails if the resulting numerator/denominator does not fit in `int`. *)
  fun to_ratvec (qs: rat list) : ratvec =
    let
      fun gcd (a: IntInf.int, b: IntInf.int) =
        let
          val a = IntInf.abs a
          val b = IntInf.abs b
          fun loop (x, 0) = x
            | loop (x, y) = loop (y, IntInf.mod (x, y))
        in
          if a = 0 then b else loop (a, b)
        end

      fun lcm (a: IntInf.int, b: IntInf.int) : IntInf.int =
        if a = 0 orelse b = 0 then 0 else IntInf.div (IntInf.abs (a * b), gcd (a, b))

      val qs = List.map normalizeRat qs
      val dens = List.map #den qs
      val l = List.foldl lcm 1 dens
      val nums =
        List.map
          (fn q =>
             let
               val scale = IntInf.div (l, #den q)
             in
               #num q * scale
             end)
          qs

      val g = List.foldl gcd l nums
      val l' = if g = 0 then l else IntInf.div (l, g)
      val nums' = if g = 0 then nums else List.map (fn n => IntInf.div (n, g)) nums
    in
      Lattice.ratvecNormalize {den = intOfIntInf l', nums = List.map intOfIntInf nums'}
    end

  (* Evaluate an integral functional on a rational vector. *)
  fun eval (f: functional, v: ratvec) : rat =
    let
      val () = if length f = length (#nums v) then () else raise Fail "Chopping_facets.eval: dim mismatch"
      val n = dotIntInf (f, #nums v)
    in
      BigRat.make (n, IntInf.fromInt (#den v))
    end

  (* Rank over Q of an integer matrix given by its columns. *)
  fun rankQ_cols (cols: int list list) : int =
    (case cols of
       [] => 0
     | col0 :: _ =>
         let
           val nRows = length col0
           val () = if List.all (fn c => length c = nRows) cols then () else raise Fail "Chopping_facets.rankQ_cols: ragged"
           val mat : Gaussian_elim.mat = List.map (fn col => List.map BigRat.fromInt col) cols
           val (pivots, _, _) = Gaussian_elim.split_data (Gaussian_elim.get_data mat)
         in
           length pivots
         end)

  (* ---------- convex-hull membership ---------- *)

  fun almost_LI (vs: ratvec list) : bool =
    rankQ_cols (List.map #nums vs) + 1 = length vs

  fun almost_LI_subsets (vs: ratvec list) : ratvec list list =
    if null vs then
      [[]]
    else
      let
        val r = rankQ_cols (List.map #nums vs)
        val subsets = Basic.choices_from (vs, r + 1)
      in
        List.filter almost_LI subsets
      end

  fun bad_index (vs: ratvec list) : int =
    let
      val n = length vs
      fun loop j =
        if j >= n then
          raise Fail "Chopping_facets.bad_index: no dependent prefix"
        else
          let
            val cols = List.map #nums (List.take (vs, j + 1))
            val r = rankQ_cols cols
          in
            if r = j then j else loop (j + 1)
          end
    in
      if n = 0 then raise Fail "Chopping_facets.bad_index: empty" else loop 0
    end

  fun find_basis (vs: ratvec list) : Gaussian_elim.mat * Gaussian_elim.vec =
    let
      val k = bad_index vs
      val cols = List.tabulate (k, fn i => to_rat (List.nth (vs, i)))
      val b = to_rat (List.nth (vs, k))
    in
      (cols, b)
    end

  fun in_hull_0_almost_LI (vs: ratvec list) : bool =
    if null vs then
      false
    else
      let
        val (a, b) = find_basis vs
        val sol = Gaussian_elim.a_solution (a, b)
        val zero = BigRat.zero ()
      in
        (* `.at`: `for s in solution do s>0 od.none`  i.e. no positive coeffs *)
        not (List.exists (fn q => ratGt (q, zero)) sol)
      end

  fun in_hull_0 (vs: ratvec list) : bool =
    List.exists in_hull_0_almost_LI (almost_LI_subsets vs)

  fun in_hull (vs: ratvec list, p: ratvec) : bool =
    in_hull_0 (List.map (fn v => Lattice.ratvecSub (v, p)) vs)

  (* Given a list of distinct vertices, remove those that lie in the hull of
     the remaining ones. *)
  fun vertices_of_hull (vs0: ratvec list) : ratvec list =
    let
      fun loop (vs, n) =
        if n > length vs - 1 then
          vs
        else
          let
            val v = List.nth (vs, n)
            val rest = deleteAt (vs, n)
          in
            if in_hull (rest, v) then loop (rest, n) else loop (vs, n + 1)
          end
    in
      loop (vs0, 0)
    end

  (* Edge test as in the `.at` script. *)
  fun is_edge (ps: ratvec list, i: int, j: int) : bool =
    let
      val b = ratvecMid (List.nth (ps, i), List.nth (ps, j))
      val q = deleteAt (ps, i)
    in
      not (in_hull (q, b))
    end

  (* ---------- slicing by a functional ---------- *)

  fun extract (vertices: ratvec list, f: functional, x: int, y: int) : ratvec list =
    if y < x then raise Fail "Chopping_facets.extract: y<x"
    else
      let
        val rx = BigRat.fromInt x
        val ry = BigRat.fromInt y
      in
        List.filter (fn v => ratLeq (rx, eval (f, v)) andalso ratLeq (eval (f, v), ry)) vertices
      end

  fun extractleft (vertices: ratvec list, f: functional, x: int) : ratvec list =
    let
      val rx = BigRat.fromInt x
    in
      List.filter (fn v => ratLeq (eval (f, v), rx)) vertices
    end

  fun extractright (vertices: ratvec list, f: functional, x: int) : ratvec list =
    let
      val rx = BigRat.fromInt x
    in
      List.filter (fn v => ratLeq (rx, eval (f, v))) vertices
    end

  (* Integers strictly between the min and max of a list of rationals. *)
  fun intseq (xs: rat list) : int list =
    let
      val lo = ratFloor (ratMin xs) + 1
      val hi = ratCeil (ratMax xs) - 1
    in
      if hi < lo then [] else List.tabulate (hi - lo + 1, fn k => lo + k)
    end

  (* Integers from ceil(min) through floor(max), inclusive. *)
  fun intseq_inclusive (xs: rat list) : int list =
    let
      val lo = ratCeil (ratMin xs)
      val hi = ratFloor (ratMax xs)
    in
      if hi < lo then [] else List.tabulate (hi - lo + 1, fn k => lo + k)
    end

  fun new_verts_pair (x: ratvec, y: ratvec, f: functional) : ratvec list =
    let
      val a = eval (f, x)
      val b = eval (f, y)
      val ints = intseq [a, b]
      val denom = BigRat.sub (b, a)
      fun one i =
        let
          val iRat = BigRat.fromInt i
          val t1 = BigRat.divRat (BigRat.sub (b, iRat), denom)
          val t2 = BigRat.divRat (BigRat.sub (iRat, a), denom)
          val xs = to_rat x
          val ys = to_rat y
          val coords = ListPair.mapEq (fn (xi, yi) => BigRat.add (BigRat.mul (t1, xi), BigRat.mul (t2, yi))) (xs, ys)
        in
          to_ratvec coords
        end
    in
      if BigRat.isZero denom then [] else List.map one ints
    end

  fun new_verts_pair_inclusive (x: ratvec, y: ratvec, f: functional) : ratvec list =
    let
      val a = eval (f, x)
      val b = eval (f, y)
      val ints = intseq_inclusive [a, b]
      val denom = BigRat.sub (b, a)
      fun one i =
        let
          val iRat = BigRat.fromInt i
          val t1 = BigRat.divRat (BigRat.sub (b, iRat), denom)
          val t2 = BigRat.divRat (BigRat.sub (iRat, a), denom)
          val xs = to_rat x
          val ys = to_rat y
          val coords = ListPair.mapEq (fn (xi, yi) => BigRat.add (BigRat.mul (t1, xi), BigRat.mul (t2, yi))) (xs, ys)
        in
          to_ratvec coords
        end
    in
      if BigRat.isZero denom then [] else List.map one ints
    end

  fun new_verts (vs: ratvec list, f: functional) : ratvec list =
    let
      val n = length vs
      fun loopI (i, acc) =
        if i >= n then acc
        else
          let
            fun loopJ (j, acc2) =
              if j >= n then acc2
              else if is_edge (vs, i, j) then loopJ (j + 1, new_verts_pair (List.nth (vs, i), List.nth (vs, j), f) @ acc2)
              else loopJ (j + 1, acc2)
          in
            loopI (i + 1, loopJ (i + 1, acc))
          end
    in
      loopI (0, [])
    end

  fun new_verts_inclusive (vs: ratvec list, f: functional) : ratvec list =
    let
      val n = length vs
      fun loopI (i, acc) =
        if i >= n then acc
        else
          let
            fun loopJ (j, acc2) =
              if j >= n then acc2
              else if is_edge (vs, i, j) then
                loopJ (j + 1, new_verts_pair_inclusive (List.nth (vs, i), List.nth (vs, j), f) @ acc2)
              else loopJ (j + 1, acc2)
          in
            loopI (i + 1, loopJ (i + 1, acc))
          end
    in
      loopI (0, [])
    end

  fun chop_big (vs: ratvec list, f: functional) : ratvec list list =
    let
      val e = List.map (fn v => eval (f, v)) vs
      val x = ratMin e
      val y = ratMax e
      val cx = ratCeil x
      val fy = ratFloor y
      fun slabs () =
        if fy - cx <= 0 then [] else List.tabulate (fy - cx, fn k => extract (vs, f, cx + k, cx + k + 1))
    in
      if ratIsInteger x then
        if ratIsInteger y then slabs () else slabs () @ [extractright (vs, f, fy)]
      else if ratIsInteger y then
        extractleft (vs, f, cx) :: slabs ()
      else
        extractleft (vs, f, cx) :: slabs () @ [extractright (vs, f, fy)]
    end

  fun chop_small (vs: ratvec list, f: functional) : ratvec list list =
    let
      val e = List.map (fn v => eval (f, v)) vs
      val ints = intseq e
    in
      List.map (fn n => extract (vs, f, n, n)) ints
    end

  fun chop_small_inclusive (vs: ratvec list, f: functional) : ratvec list list =
    let
      val e = List.map (fn v => eval (f, v)) vs
      val ints = intseq_inclusive e
    in
      List.map (fn n => extract (vs, f, n, n)) ints
    end

  fun chop (vs0: ratvec list, f: functional) : ratvec list list =
    let
      val e = List.map (fn v => eval (f, v)) vs0
      val x = ratMin e
      val y = ratMax e
    in
      if ratFloor x >= ratCeil y - 1 then
        [vs0]
      else
        let
          val vs = vs0 @ new_verts (vs0, f)
          val regions = chop_small (vs, f) @ chop_big (vs, f)
        in
          List.map vertices_of_hull regions
        end
    end

  fun bary (vertices: ratvec list) : ratvec =
    (case vertices of
       [] => raise Fail "Chopping_facets.bary: empty"
     | v0 :: _ =>
         let
           val l = length (#nums v0)
           val zero = {den = 1, nums = List.tabulate (l, fn _ => 0)}
           val sum = List.foldl (fn (v, acc) => ratvecAdd (acc, v)) zero vertices
         in
           Lattice.ratvecScale (sum, 1, length vertices)
         end)

  fun chop_coroots (g: RootDatum.t, vertices: ratvec list) : ratvec list list =
    let
      val coroots = RootDatum.posCorootsCols g
      fun step (c, regions) = List.concat (List.map (fn r => chop (r, c)) regions)
    in
      List.foldl step [vertices] coroots
    end

  fun chop_coroots_verbose (g: RootDatum.t, vertices: ratvec list) : unit =
    let
      val coroots = RootDatum.posCorootsCols g
      fun appi f xs =
        let
          fun loop (_, []) = ()
            | loop (i, x :: rest) = (f (i, x); loop (i + 1, rest))
        in
          loop (0, xs)
        end
      fun step (c, (i, regions)) =
        let
          val regions' = List.concat (List.map (fn r => chop (r, c)) regions)
          val () = TextIO.print ("coroot " ^ Int.toString i ^ ": " ^ Int.toString (length regions') ^ " convex regions\n")
        in
          (i + 1, regions')
        end

      val (_, out) = List.foldl step (0, [vertices]) coroots
      val () = TextIO.print "Final output:\n"
      fun show (i, r) =
        TextIO.print
          ( "Region " ^ Int.toString i ^ ": " ^ Int.toString (length r)
            ^ " vertices, barycenter = "
            ^ Int.toString (#den (bary r))
            ^ "/... (ratvec)\n"
          )
      val () = appi show out
    in
      ()
    end

  (* Stub: the `.at` file also defines `max_levis`, but it depends on
     `Levi_subgroups` from `subgroups.at`. *)
  fun max_levis (_: RootDatum.t) : RootDatum.t list =
    raise Fail "Chopping_facets.max_levis: unimplemented (needs Levi_subgroups)"
end
