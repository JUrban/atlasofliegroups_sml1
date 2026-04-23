use "atlas-scripts-sml/basic.sml";
use "atlas-scripts-sml/Lattice.sml";
use "atlas-scripts-sml/RootDatum.sml";

(*
  File: atlas-scripts-sml/misc.sml

  Purpose
  - Partial SML translation of `atlas-scripts/misc.at`.
  - The original `.at` file is a grab bag of utilities. This port starts with
    the parts that are broadly useful as dependencies for other scripts:
      - Cartesian-product enumerators (`box`, `all_words`)
      - bit / subset helpers (`to_binary`, `generate_all_subsets`)
      - list cleanup helpers (`delete_trailing_zeros`, `delete_leading_zeros`)
      - small root-system helpers (`height`, `sgn`, `root_string`, ...)

  Scope / limitations
  - This is intentionally incremental; many representation-theoretic helpers in
    `misc.at` depend on other large `.at` modules and are not yet ported here.
*)

structure Misc = struct
  type vec = int list
  type ratvec = Lattice.ratvec
  type rootdatum = RootDatum.t

  (* `all_words(heights)` = Cartesian product [0..heights[0]-1]×... *)
  fun all_words (heights: int list) : int list list =
    let
      fun loop [] = [[]]
        | loop (h :: hs) =
            if h < 0 then raise Fail "Misc.all_words: negative height"
            else
              let
                val tails = loop hs
              in
                List.concat (List.tabulate (h, fn i => List.map (fn t => i :: t) tails))
              end
    in
      loop heights
    end

  (* `box(height,rank)` from `misc.at`: all vectors of length `rank` with entries < height. *)
  fun box (height: int, rank: int) : vec list =
    if rank < 0 then raise Fail "Misc.box: negative rank"
    else all_words (List.tabulate (rank, fn _ => height))

  (* `box(heights)` from `misc.at`. *)
  fun box_heights (heights: int list) : vec list = all_words heights

  (* Flatten a list of lists. *)
  fun flatten (xss: 'a list list) : 'a list = List.concat xss

  (* Convert a nonnegative integer to a binary vector of fixed length (LSB at end). *)
  fun to_binary (length: int, n: int) : vec =
    if length < 0 then raise Fail "Misc.to_binary: negative length"
    else if n < 0 then raise Fail "Misc.to_binary: negative n"
    else
      let
        val v = Array.array (length, 0)
        fun loop (k, m) =
          if k = 0 orelse m = 0 then ()
          else
            let
              val q = m div 2
              val r = m mod 2
              val idx = k - 1
            in
              Array.update (v, idx, r);
              loop (k - 1, q)
            end
        val () = loop (length, n)
      in
        Array.foldr (op ::) [] v
      end

  (* Generate all subsets of a list (power set). *)
  val generate_all_subsets = Basic.power_set

  fun is_proper_subset (a: int list, b: int list) : bool =
    let
      fun member x xs = List.exists (fn y => y = x) xs
      val sub = List.all (fn x => member x b) a
      val strict = not (List.all (fn x => member x a) b)
    in
      sub andalso strict
    end

  (* Delete trailing zeros from an int list. *)
  fun delete_trailing_zeros (xs: int list) : int list =
    let
      fun dropWhile0 ys =
        (case ys of
           [] => []
         | 0 :: rest => dropWhile0 rest
         | _ => ys)
    in
      List.rev (dropWhile0 (List.rev xs))
    end

  (* Delete leading zeros from an int list. *)
  fun delete_leading_zeros (xs: int list) : int list =
    let
      fun dropWhile0 ys =
        (case ys of
           [] => []
         | 0 :: rest => dropWhile0 rest
         | _ => ys)
    in
      dropWhile0 xs
    end

  (* --- Root helpers (from `misc.at`) ---------------------------------- *)

  fun is_root (rd: rootdatum, alpha: vec) : bool =
    let
      val npr = RootDatum.numPosRoots rd
      val i = RootDatum.rootIndex (rd, alpha)
    in
      i <> npr
    end

  fun is_posroot (rd: rootdatum, alpha: vec) : bool =
    let
      val npr = RootDatum.numPosRoots rd
      val i = RootDatum.rootIndex (rd, alpha)
    in
      i >= 0 andalso i < npr
    end

  fun not_a_root (rd: rootdatum, alpha: vec) : bool = not (is_root (rd, alpha))

  fun all_root_index (rd: rootdatum, alpha: vec) : int =
    let
      fun findIndex ([], _) = ~1
        | findIndex (x :: xs, i) = if x = alpha then i else findIndex (xs, i + 1)
    in
      findIndex (RootDatum.rootsCols rd, 0)
    end

  fun simple_root_index (rd: rootdatum, alpha: vec) : int =
    let
      fun findIndex ([], _) = ~1
        | findIndex (x :: xs, i) = if x = alpha then i else findIndex (xs, i + 1)
    in
      findIndex (RootDatum.simpleRootsCols rd, 0)
    end

  fun pm_simple_root_index (rd: rootdatum, alpha: vec) : int =
    if is_posroot (rd, alpha) then simple_root_index (rd, alpha) else simple_root_index (rd, List.map (fn x => ~x) alpha)

  fun sgn (rd: rootdatum, alpha: vec) : int =
    if not (is_root (rd, alpha)) then raise Fail "Misc.sgn: not a root"
    else if is_posroot (rd, alpha) then 1 else ~1

  (* Root height: sum of simple-root coefficients (absolute value for negatives). *)
  fun height (rd: rootdatum, alpha: vec) : int =
    if not (is_root (rd, alpha)) then raise Fail "Misc.height: not a root"
    else
      let
        val coeffs = RootDatum.rootExpression rd alpha
        val h = List.foldl (op +) 0 coeffs
      in
        Int.abs h
      end

  fun roots_of_height (rd: rootdatum, h: int) : vec list =
    List.filter (fn alpha => height (rd, alpha) = h) (RootDatum.rootsCols rd)

  fun max_height (rd: rootdatum) : int =
    let
      val pos = RootDatum.posRootsCols rd
    in
      case pos of
        [] => 0
      | a :: rest => List.foldl (fn (x, acc) => Int.max (acc, height (rd, x))) (height (rd, a)) rest
    end

  fun roots_by_height (rd: rootdatum) : vec list list =
    let
      val mh = max_height rd
    in
      List.tabulate (mh + 1, fn i => roots_of_height (rd, i))
    end

  fun vecScale (k: int, xs: vec) : vec = List.map (fn a => k * a) xs
  fun vecAdd (xs: vec, ys: vec) : vec = ListPair.mapEq (op +) (xs, ys)
  fun vecSub (xs: vec, ys: vec) : vec = ListPair.mapEq (op -) (xs, ys)

  (* Return (p,q) where alpha+p*beta is a root (max p) and alpha-q*beta is a root (max q). *)
  fun root_string (rd: rootdatum, beta: vec, alpha: vec) : int * int =
    let
      val () =
        if alpha = List.map (fn x => ~x) beta then raise Fail "Misc.root_string: alpha = -beta" else ()
      fun isRoot v = is_root (rd, v)
      fun firstNot limit f =
        let
          fun loop i = if i > limit then limit + 1 else if f i then loop (i + 1) else i
        in
          loop 0
        end
      val limit = 4
      val x = firstNot limit (fn i => isRoot (vecAdd (alpha, vecScale (i, beta))))
      val y = firstNot limit (fn i => isRoot (vecSub (alpha, vecScale (i, beta))))
    in
      (x - 1, y - 1)
    end

  (* Write a positive root as a sum of simple roots (returned as a list of simple roots). *)
  fun posroot_as_sum_of_simple (rd: rootdatum, alpha: vec) : vec list =
    if not (is_root (rd, alpha)) then raise Fail "Misc.posroot_as_sum_of_simple: not a root"
    else if not (is_posroot (rd, alpha)) then raise Fail "Misc.posroot_as_sum_of_simple: expected a positive root"
    else
      let
        val coeffs = RootDatum.rootExpression rd alpha
        val simps = RootDatum.simpleRootsCols rd
        val () =
          if length coeffs = length simps then ()
          else raise Fail "Misc.posroot_as_sum_of_simple: rank mismatch"
        fun repeat (0, _) = []
          | repeat (k, v) = v :: repeat (k - 1, v)
        fun step ((c, v), acc) =
          if c < 0 then raise Fail "Misc.posroot_as_sum_of_simple: negative coefficient"
          else acc @ repeat (c, v)
      in
        List.foldl step [] (ListPair.zipEq (coeffs, simps))
      end

  (* Write any root as a sum of simple roots (or as sum of negative simples). *)
  fun root_as_sum_of_simple (rd: rootdatum, alpha: vec) : vec list =
    if not (is_root (rd, alpha)) then raise Fail "Misc.root_as_sum_of_simple: not a root"
    else
      let
        val coeffs = RootDatum.rootExpression rd alpha
        val simps = RootDatum.simpleRootsCols rd
        val () =
          if length coeffs = length simps then ()
          else raise Fail "Misc.root_as_sum_of_simple: rank mismatch"
        fun repeat (0, _) = []
          | repeat (k, v) = v :: repeat (k - 1, v)
        val allNonNeg = List.all (fn c => c >= 0) coeffs
        val allNonPos = List.all (fn c => c <= 0) coeffs
        fun stepPos ((c, v), acc) = acc @ repeat (c, v)
        fun stepNeg ((c, v), acc) = acc @ repeat (~c, List.map (fn x => ~x) v)
      in
        if allNonNeg then
          List.foldl stepPos [] (ListPair.zipEq (coeffs, simps))
        else if allNonPos then
          List.foldl stepNeg [] (ListPair.zipEq (coeffs, simps))
        else
          raise Fail "Misc.root_as_sum_of_simple: mixed-sign simple expansion"
      end

  (* Return a simple-root summand of `alpha`, with the same sign as `alpha`. *)
  fun simple_root_summand (rd: rootdatum, alpha: vec) : vec =
    if not (is_root (rd, alpha)) then raise Fail "Misc.simple_root_summand: not a root"
    else
      let
        val simps = RootDatum.simpleRootsCols rd
        val simpCoroots = RootDatum.simpleCorootsCols rd
        val () =
          if length simps = length simpCoroots then ()
          else raise Fail "Misc.simple_root_summand: mismatch"
        fun firstIndex pred =
          let
            fun loop ([], _) = raise Fail "Misc.simple_root_summand: no index found"
              | loop (v :: vs, i) = if pred v then i else loop (vs, i + 1)
          in
            loop (simpCoroots, 0)
          end
        val i =
          if is_posroot (rd, alpha) then
            firstIndex (fn alphav => RootDatum.dot (alpha, alphav) > 0)
          else
            firstIndex (fn alphav => RootDatum.dot (alpha, alphav) < 0)
        val si = List.nth (simps, i)
      in
        if is_posroot (rd, alpha) then si else List.map (fn x => ~x) si
      end
end
