use "atlas-scripts-sml/Lattice.sml";
use "atlas-scripts-sml/RootDatum.sml";

(*
  File: atlas-scripts-sml/chopping_facets_fast.sml

  Purpose
  - SML translation of `atlas-scripts/chopping_facets_fast.at`.
  - Provides basic routines for subdividing a set of rational vertices by
    integer translates of a linear functional (typically a coroot).

  Background (Atlas `.at` intent)
  - Given a list `V` of vertices (ratvecs) and an integral functional `f` (vec),
    the script:
      - creates new vertices on line segments so that `f(p)` becomes integral
        at the new points (subject to a denominator bound),
      - then partitions the vertex set into regions cut by the hyperplanes
        `f = n` for integers `n`.

  Types
  - `ratvec` is `Lattice.ratvec` = `{den:int, nums:int list}` (common denominator).
  - `functional` is an integral vector `int list`.

  Notes / limitations
  - This port uses `int` arithmetic like the `.at` version; it may overflow on
    very large numerators/denominators.
*)

structure Chopping_facets_fast = struct
  type ratvec = Lattice.ratvec
  type functional = int list

  structure Rat = struct
    type t = {num: int, den: int}

    fun gcd (a: int, b: int) : int =
      let
        val a = Int.abs a
        val b = Int.abs b
        fun loop (x, 0) = x
          | loop (x, y) = loop (y, x mod y)
      in
        if a = 0 then b else loop (a, b)
      end

    fun normalize ({num, den}: t) : t =
      if den = 0 then raise Fail "Rat.normalize: zero denom"
      else
        let
          val sign = if den < 0 then ~1 else 1
          val num1 = num * sign
          val den1 = den * sign
          val g = gcd (num1, den1)
        in
          {num = num1 div g, den = den1 div g}
        end

    fun fromInt n : t = {num = n, den = 1}

    fun add (a: t, b: t) : t =
      let
        val a = normalize a
        val b = normalize b
      in
        normalize {num = #num a * #den b + #num b * #den a, den = #den a * #den b}
      end

    fun sub (a: t, b: t) : t =
      add (a, {num = ~ (#num b), den = #den b})

    fun mul (a: t, b: t) : t =
      let
        val a = normalize a
        val b = normalize b
      in
        normalize {num = #num a * #num b, den = #den a * #den b}
      end

    fun divRat (a: t, b: t) : t =
      let
        val b = normalize b
        val () = if #num b = 0 then raise Fail "Rat.div: divide by zero" else ()
      in
        mul (a, {num = #den b, den = #num b})
      end

    fun leq (a: t, b: t) : bool =
      let
        val a = normalize a
        val b = normalize b
      in
        #num a * #den b <= #num b * #den a
      end

    fun lt (a: t, b: t) : bool = leq (a, b) andalso not (leq (b, a))

    fun max (xs: t list) : t =
      (case xs of
         [] => raise Fail "Rat.max: empty"
       | x :: rest => List.foldl (fn (y, acc) => if leq (acc, y) then y else acc) x rest)

    fun min (xs: t list) : t =
      (case xs of
         [] => raise Fail "Rat.min: empty"
       | x :: rest => List.foldl (fn (y, acc) => if leq (y, acc) then y else acc) x rest)

    fun is_integer (a: t) : bool = (#den (normalize a) = 1)

    fun floor (a: t) : int =
      let
        val a = normalize a
        val n = #num a
        val d = #den a
      in
        if n >= 0 then Int.div (n, d) else ~ (Int.div (~n + d - 1, d))
      end

    fun ceil (a: t) : int =
      let
        val a = normalize a
        val n = #num a
        val d = #den a
      in
        if n >= 0 then Int.div (n + d - 1, d) else ~ (Int.div (~n, d))
      end

    fun denom (a: t) : int = #den (normalize a)
  end

  fun dot (xs: int list, ys: int list) : int =
    List.foldl (op +) 0 (ListPair.mapEq (op *) (xs, ys))

  fun eval (f: functional, v: ratvec) : Rat.t =
    let
      val () = if length f = length (#nums v) then () else raise Fail "Chopping_facets_fast.eval: dim mismatch"
      val n = dot (f, #nums v)
    in
      Rat.normalize {num = n, den = #den v}
    end

  fun ratvecNeg (v: ratvec) : ratvec = Lattice.ratvecScale (v, ~1, 1)
  fun ratvecAdd (a: ratvec, b: ratvec) : ratvec = Lattice.ratvecSub (a, ratvecNeg b)

  fun ratvecScaleRat (v: ratvec, r: Rat.t) : ratvec =
    let
      val r = Rat.normalize r
    in
      Lattice.ratvecScale (v, #num r, #den r)
    end

  (* extract vertices with x <= f*v <= y *)
  fun extract (vertices: ratvec list, f: functional, x: int, y: int) : ratvec list =
    if y < x then raise Fail "extract: y<x"
    else
      List.filter
        (fn v =>
           let
             val a = eval (f, v)
           in
             Rat.leq (Rat.fromInt x, a) andalso Rat.leq (a, Rat.fromInt y)
           end)
        vertices

  fun extractleft (vertices: ratvec list, f: functional, x: int) : ratvec list =
    List.filter (fn v => Rat.leq (eval (f, v), Rat.fromInt x)) vertices

  fun extractright (vertices: ratvec list, f: functional, x: int) : ratvec list =
    List.filter (fn v => Rat.leq (Rat.fromInt x, eval (f, v))) vertices

  (* All integers strictly between `min(list)` and `max(list)`. *)
  fun int_between (xs: Rat.t list) : int list =
    let
      val mn = Rat.min xs
      val mx = Rat.max xs
      val lo = Rat.floor mn + 1
      val hi = Rat.ceil mx - 1
    in
      if hi < lo then [] else List.tabulate (hi - lo + 1, fn k => lo + k)
    end

  (* New vertices on the segment xy where f(pi) is integral and denom(pi) <= N. *)
  fun new_verts_pair (x: ratvec, y: ratvec, f: functional, nBound: int) : ratvec list =
    let
      val a = eval (f, x)
      val b = eval (f, y)
      val ints = int_between [a, b]
      val denomBA = Rat.sub (b, a)
      fun one i =
        let
          val iRat = Rat.fromInt i
          val t1 = Rat.divRat (Rat.sub (b, iRat), denomBA) (* (b-i)/(b-a) *)
          val t2 = Rat.divRat (Rat.sub (iRat, a), denomBA) (* (i-a)/(b-a) *)
          val p = ratvecAdd (ratvecScaleRat (x, t1), ratvecScaleRat (y, t2))
          val p = Lattice.ratvecNormalize p
        in
          if #den p <= nBound then SOME p else NONE
        end
    in
      List.mapPartial one ints
    end

  fun new_verts (vs: ratvec list, f: functional, nBound: int) : ratvec list =
    let
      val n = length vs
      fun loopI (i, acc) =
        if i >= n then acc
        else
          let
            val xi = List.nth (vs, i)
            fun loopJ (j, acc2) =
              if j >= n then acc2
              else
                let
                  val yj = List.nth (vs, j)
                in
                  loopJ (j + 1, new_verts_pair (xi, yj, f, nBound) @ acc2)
                end
          in
            loopI (i + 1, loopJ (i + 1, acc))
          end
    in
      loopI (0, [])
    end

  fun chop_big (vs: ratvec list, f: functional) : ratvec list list =
    let
      val e = List.map (fn v => eval (f, v)) vs
      val x = Rat.min e
      val y = Rat.max e
      val cx = Rat.ceil x
      val fy = Rat.floor y
      fun slabs () =
        if fy - cx <= 0 then []
        else List.tabulate (fy - cx, fn k => extract (vs, f, cx + k, cx + k + 1))
    in
      if Rat.is_integer x then
        if Rat.is_integer y then
          slabs ()
        else
          slabs () @ [extractright (vs, f, fy)]
      else if Rat.is_integer y then
        [extractleft (vs, f, cx)] @ slabs ()
      else
        [extractleft (vs, f, cx)] @ slabs () @ [extractright (vs, f, fy)]
    end

  fun chop_small (vs: ratvec list, f: functional) : ratvec list list =
    let
      val e = List.map (fn v => eval (f, v)) vs
      val ints = int_between e
    in
      List.map (fn n => extract (vs, f, n, n)) ints
    end

  fun clean (regions: ratvec list list) : ratvec list list =
    List.filter (fn r => not (null r)) regions

  fun chop (vs: ratvec list, f: functional, nBound: int) : ratvec list list =
    let
      val e = List.map (fn v => eval (f, v)) vs
      val x = Rat.min e
      val y = Rat.max e
    in
      if Rat.floor x >= Rat.ceil y - 1 then
        [vs]
      else
        let
          val vs2 = vs @ new_verts (vs, f, nBound)
        in
          clean (chop_small (vs2, f) @ chop_big (vs2, f))
        end
    end

  (* Apply `chop` iteratively across all positive coroots. *)
  fun chop_coroots (rd: RootDatum.t, vertices: ratvec list, nBound: int) : ratvec list list =
    let
      val coroots = RootDatum.posCorootsCols rd
      fun step (c, regions) = List.concat (List.map (fn r => chop (r, c, nBound)) regions)
    in
      List.foldl step [vertices] coroots
    end
end
