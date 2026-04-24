use "atlas-scripts-sml/MatrixAT.sml";
use "atlas-scripts-sml/IntMatrix.sml";
use "atlas-scripts-sml/basic.sml";
use "atlas-scripts-sml/partitions.sml";

(*
  File: atlas-scripts-sml/combinatorics.sml

  Purpose
  - Partial SML analogue of `atlas-scripts/combinatorics.at`, focused on
    permutation utilities used by the diagram/folding code.
*)
structure Combinatorics = struct
  type permutation = int list
  type mat = IntMatrix.mat
  type partition = Partitions.partition
  type bipartition = partition * partition
  type signed_cycle = int * bool
  type signed_cycles = signed_cycle list
  (* A Symbol is the 2-row combinatorial gadget used in Springer theory for
     types B/C/D; it is a transformed bipartition representation. *)
  type symbol = int list * int list
  datatype D_class =
    D_unsplit_class of signed_cycles
  | D_split_class of partition * bool
  datatype D_irrep =
    D_unsplit_irr of bipartition
  | D_split_irr of partition * bool

  val is_permutation = MatrixAT.is_permutation

  (* Permutation matrix for `pi`. *)
  fun permutation_matrix (pi: permutation) : mat =
    MatrixAT.permutation_matrix pi

  (* Composition `sigma ∘ pi` (apply `pi` then `sigma`). *)
  fun compose_permutations (sigma: permutation, pi: permutation) : permutation =
    let
      val n = length sigma
      val () = if length pi = n then () else raise Fail "Combinatorics.compose_permutations: length mismatch"
      fun at xs i = List.nth (xs, i)
    in
      List.map (fn i => at sigma i) pi
    end

  (* Inverse permutation, validating input. *)
  fun inverse (pi: permutation) : permutation =
    if not (is_permutation pi) then
      raise Fail "Combinatorics.inverse: not a permutation"
    else
      let
        val n = length pi
        val out = Array.array (n, 0)
        fun loop (j, []) = ()
          | loop (j, x :: xs) = (Array.update (out, x, j); loop (j + 1, xs))
        val () = loop (0, pi)
      in
        List.tabulate (n, fn i => Array.sub (out, i))
      end

  (* Inverse permutation without validation (assumes `pi` is a permutation). *)
  fun permutation_inverse (pi: permutation) : permutation =
    let
      val n = length pi
      val out = Array.array (n, 0)
      fun loop (j, []) = ()
        | loop (j, x :: xs) = (Array.update (out, x, j); loop (j + 1, xs))
      val () = loop (0, pi)
    in
      List.tabulate (n, fn i => Array.sub (out, i))
    end

  (* Construct the permutation that cycles the listed elements and fixes others. *)
  fun cyclic_permutation (n: int) (cycle: int list) : permutation =
    if n < 0 then
      raise Fail "Combinatorics.cyclic_permutation: negative n"
    else
      let
        val pi = Array.tabulate (n, fn i => i) (* identity *)
        val l = length cycle
        val () = if l = 0 then () else ()
        fun nth xs i = List.nth (xs, i)
        fun loop (idx, []) = ()
          | loop (idx, e :: es) =
              let
                val next = nth cycle ((idx + 1) mod l)
              in
                Array.update (pi, e, next);
                loop (idx + 1, es)
              end
        val () =
          if l = 0 then ()
          else if List.all (fn e => 0 <= e andalso e < n) cycle then loop (0, cycle)
          else raise Fail "Combinatorics.cyclic_permutation: cycle out of range"
      in
        List.tabulate (n, fn i => Array.sub (pi, i))
      end

  (* ---------- binomials and combination coding (from `combinatorics.at`) ---------- *)

  (* Binomial coefficient as `IntInf.int` to reduce overflow risk. *)
  fun binomIntInf (n: int, k0: int) : IntInf.int =
    if n < 0 then
      raise Fail "Combinatorics.binom: n<0"
    else
      let
        val k0 = if k0 < 0 orelse k0 > n then ~1 else k0
      in
        if k0 < 0 then
          0
        else
          let
            val k = if k0 + k0 > n then n - k0 else k0
          in
            if k = 0 then 1
            else if k = 1 then IntInf.fromInt n
            else
              let
                val nI = IntInf.fromInt n
                fun divExact (a: IntInf.int, d: int) : IntInf.int =
                  let
                    val dI = IntInf.fromInt d
                    val (q, r) = IntInf.divMod (a, dI)
                  in
                    if r = 0 then q else raise Fail "Combinatorics.binom: inexact division"
                  end
                (* Mimic the `.at` recurrence:
                     p = n*(n-1)
                     for i=2..k-1: p := (p / i) * (n-i)
                     result = p / k
                *)
                fun loop (i: int, p: IntInf.int) : IntInf.int =
                  if i > k - 1 then p
                  else
                    let
                      val p' = divExact (p, i) * (nI - IntInf.fromInt i)
                    in
                      loop (i + 1, p')
                    end
                val p0 = nI * (nI - 1)
                val p1 = loop (2, p0)
              in
                divExact (p1, k)
              end
          end
      end

  (* Binomial coefficient as `int`, raising if it does not fit. *)
  fun binom (n: int, k: int) : int =
    let
      val x = binomIntInf (n, k)
    in
      if x > IntInf.fromInt (Option.valOf Int.maxInt) then
        raise Fail "Combinatorics.binom: overflow"
      else
        IntInf.toInt x
    end

  (* Encode a strictly increasing k-combination [c0<c1<...<c_{k-1}] into N. *)
  fun combination_encode (cs: int list) : int =
    let
      fun step (c, i, acc) = acc + binomIntInf (c, i + 1)
      fun loop ([], _, acc) = acc
        | loop (c :: rest, i, acc) = loop (rest, i + 1, step (c, i, acc))
      val nI = loop (cs, 0, 0)
    in
      if nI > IntInf.fromInt (Option.valOf Int.maxInt) then
        raise Fail "Combinatorics.combination_encode: overflow"
      else
        IntInf.toInt nI
    end

  (* Decode with fixed k: returns a strictly increasing list. *)
  fun combination_decode (k: int) : int -> int list =
    if k < 0 then
      raise Fail "Combinatorics.combination_decode: negative k"
    else
      fn n0 =>
        if k = 0 then
          []
        else
          let
            val nRef = ref (IntInf.fromInt n0)
            val boundRef = ref (n0 + k) (* conservative bound as in `.at` *)
            val out = Array.array (k, 0)
            fun step i =
              if i = 0 then
                ()
              else
                let
                  val bound = !boundRef
                  val ci =
                    Basic.binary_search_first
                      ( fn n =>
                          binomIntInf (n, i) > !nRef
                      , 1
                      , bound
                      )
                    - 1
                  val () = Array.update (out, i - 1, ci)
                  val () = nRef := !nRef - binomIntInf (ci, i)
                  val () = boundRef := ci
                in
                  step (i - 1)
                end
            val () = step k
          in
            List.tabulate (k, fn i => Array.sub (out, i))
          end

  (* Multinomial coefficient: product over i of binom(upper[i], a[i]) where
     upper[i] is the suffix-sum of a starting at i. Result as IntInf. *)
  fun multinomIntInf (a: int list) : IntInf.int =
    let
      val () = if List.all (fn x => x >= 0) a then () else raise Fail "Combinatorics.multinom: negative part"
      fun suffixSums xs =
        let
          fun loop ([], acc) = acc
            | loop (x :: rest, acc) =
                (case acc of
                   [] => loop (rest, [x])
                 | s :: _ => loop (rest, (x + s) :: acc))
        in
          loop (List.rev xs, [])
        end
      val uppers = suffixSums a
      fun step ((m, upper), acc) = acc * binomIntInf (upper, m)
    in
      List.foldl step 1 (ListPair.zipEq (a, uppers))
    end

  fun multi_chooseIntInf (n: int, k: int) : IntInf.int =
    if n <= 0 orelse k < 0 then raise Fail "Combinatorics.multi_choose: bad args"
    else binomIntInf (n + k - 1, k)

  fun falling_powerIntInf (n: int, k: int) : IntInf.int =
    if k < 0 then raise Fail "Combinatorics.falling_power: negative exponent"
    else
      let
        fun loop (i, acc) =
          if i > k then acc
          else loop (i + 1, acc * IntInf.fromInt (n - k + i))
      in
        loop (1, 1)
      end

  fun rising_powerIntInf (n: int, k: int) : IntInf.int =
    if k < 0 then raise Fail "Combinatorics.rising_power: negative exponent"
    else
      let
        fun loop (i, acc) =
          if i >= k then acc
          else loop (i + 1, acc * IntInf.fromInt (n + i))
      in
        loop (0, 1)
      end

  fun even_places (xs: 'a list) : 'a list =
    let
      fun loop ([], _, acc) = List.rev acc
        | loop (x :: rest, i, acc) =
            if i mod 2 = 0 then loop (rest, i + 1, x :: acc) else loop (rest, i + 1, acc)
    in
      loop (xs, 0, [])
    end

  fun odd_places (xs: 'a list) : 'a list =
    let
      fun loop ([], _, acc) = List.rev acc
        | loop (x :: rest, i, acc) =
            if i mod 2 = 1 then loop (rest, i + 1, x :: acc) else loop (rest, i + 1, acc)
    in
      loop (xs, 0, [])
    end

  (* Product of a list of permutations, matching `permutation_product` in `.at`:
     permutations are composed left-to-right (p0 ∘ p1 ∘ ...). *)
  fun permutation_product (ps: permutation list) : permutation =
    (case ps of
       [] => []
     | p0 :: _ =>
         let
           val n = length p0
           val () = if List.all (fn p => length p = n) ps then () else raise Fail "Combinatorics.permutation_product: length mismatch"
           val () = if List.all is_permutation ps then () else raise Fail "Combinatorics.permutation_product: non-permutation"
           val id = List.tabulate (n, fn i => i)
           fun apply (p: permutation, x: int) = List.nth (p, x)
           fun compose (acc: permutation, p: permutation) : permutation =
             List.map (fn x => apply (acc, apply (p, x))) id
         in
           List.foldl compose id ps
         end)

  (* Product of many cycles (disjoint or not), in the same order as `.at`. *)
  fun cycle_product (n: int) (cycles: int list list) : permutation =
    permutation_product (List.map (cyclic_permutation n) cycles)

  (* Permutation action on an int vector/list, matching `.at` convention:
       result[pi[j]] = v[j].
  *)
  fun permute_vec (pi: permutation, v: int list) : int list =
    if length pi <> length v orelse not (is_permutation pi) then
      raise Fail "Combinatorics.permute_vec: size/permutation mismatch"
    else
      let
        val n = length v
        val out = Array.array (n, 0)
        fun loop (j, []) = ()
          | loop (j, pj :: rest) = (Array.update (out, pj, List.nth (v, j)); loop (j + 1, rest))
        val () = loop (0, pi)
      in
        List.tabulate (n, fn i => Array.sub (out, i))
      end

  fun permute_list (pi: permutation, xs: 'a list) : 'a list =
    if length pi <> length xs orelse not (is_permutation pi) then
      raise Fail "Combinatorics.permute_list: size/permutation mismatch"
    else
      let
        val n = length xs
        val out = Array.array (n, List.nth (xs, 0))
        fun loop (j, []) = ()
          | loop (j, pj :: rest) = (Array.update (out, pj, List.nth (xs, j)); loop (j + 1, rest))
        val () = loop (0, pi)
      in
        List.tabulate (n, fn i => Array.sub (out, i))
      end

  (* ---------------- symmetric group helpers (ported from `combinatorics.at`) ---------------- *)

  fun sumInts (xs: int list) : int = List.foldl (op +) 0 xs

  fun productIntInf (xs: int list) : IntInf.int =
    List.foldl (fn (x, acc) => acc * IntInf.fromInt x) 1 xs

  fun factorialIntInf (n: int) : IntInf.int =
    if n < 0 then raise Fail "Combinatorics.factorialIntInf: negative"
    else
      let
        fun loop (k, acc) = if k <= 1 then acc else loop (k - 1, acc * IntInf.fromInt k)
      in
        loop (n, 1)
      end

  fun toIntChecked (where', x: IntInf.int) : int =
    let
      val maxI = IntInf.fromInt (Option.valOf Int.maxInt)
      val minI = IntInf.fromInt (Option.valOf Int.minInt)
    in
      if x > maxI orelse x < minI then
        raise Fail ("Combinatorics." ^ where' ^ ": overflow")
      else
        IntInf.toInt x
    end

  fun gcd (a: int, b: int) : int =
    if b = 0 then Int.abs a else gcd (b, a mod b)

  fun lcm_list (xs: int list) : int =
    let
      fun lcm2 (a, b) =
        if a = 0 orelse b = 0 then 0 else Int.abs (a div gcd (a, b) * b)
    in
      List.foldl lcm2 1 xs
    end

  fun frequencies (xs: int list) : int list =
    let
      fun loop ([], NONE, c, acc) = List.rev acc
        | loop ([], SOME _, c, acc) = List.rev (c :: acc)
        | loop (x :: rest, NONE, _, acc) = loop (rest, SOME x, 1, acc)
        | loop (x :: rest, SOME y, c, acc) =
            if x = y then loop (rest, SOME y, c + 1, acc)
            else loop (rest, SOME x, 1, c :: acc)
    in
      loop (xs, NONE, 0, [])
    end

  val cycle_type_order = lcm_list

  fun strip_to_partition (parts: int list) : partition =
    Basic.sort (fn (a, b) => a >= b) (List.filter (fn x => x > 0) parts)

  (* Lexicographic comparison restricted to partitions of the same sum, with
     implicit 0-extension (ported from `rlex_leq_partitions` in `.at`). *)
  fun rlex_leq_partitions (lambda: partition, mu: partition) : bool =
    let
      val m = length mu
      fun loop ([], i) =
        if i >= m then true
        else
          let
            val rest = List.drop (mu, i)
          in
            if List.all (fn x => x = 0) rest then true else raise Fail "Combinatorics.rlex_leq_partitions: unequal sum"
          end
        | loop (l :: ls, i) =
            if i >= m then
              if l = 0 then loop (ls, i + 1) else raise Fail "Combinatorics.rlex_leq_partitions: unequal sum"
            else
              let
                val mi = List.nth (mu, i)
              in
                if l <> mi then l < mi else loop (ls, i + 1)
              end
    in
      loop (lambda, 0)
    end

  (* Three-way comparison result in {-1,0,1} for equal-sum partitions, without
     0-extension (ported from `rlex_cmp_partitions` in `.at`). *)
  fun rlex_cmp_partitions (lambda: partition, mu: partition) : int =
    let
      fun loop ([], []) = 0
        | loop (l :: ls, m :: ms) = if l <> m then (if l < m then ~1 else 1) else loop (ls, ms)
        | loop _ = 0
    in
      loop (lambda, mu)
    end

  (* Compare partitions by sum then lexicographically (ported from `slex_*` in `.at`). *)
  fun slex_leq_partitions (lambda: partition, mu: partition) : bool =
    let
      val sl = sumInts lambda
      val sm = sumInts mu
    in
      if sl <> sm then sl < sm else rlex_leq_partitions (lambda, mu)
    end

  fun slex_cmp_partitions (lambda: partition, mu: partition) : int =
    let
      val sl = sumInts lambda
      val sm = sumInts mu
    in
      if sl < sm then ~1 else if sl > sm then 1 else rlex_cmp_partitions (lambda, mu)
    end

  fun cycle_centralizer_order_IntInf (cycles: int list) : IntInf.int =
    let
      val freqs = frequencies cycles
      val multPart = List.foldl (fn (m, acc) => acc * factorialIntInf m) 1 freqs
    in
      productIntInf cycles * multPart
    end

  fun cycle_class_size (cycles: int list) : int =
    let
      val n = sumInts cycles
      val denom = cycle_centralizer_order_IntInf cycles
      val num = factorialIntInf n
      val (q, r) = IntInf.divMod (num, denom)
      val () = if r = 0 then () else raise Fail "Combinatorics.cycle_class_size: inexact"
    in
      toIntChecked ("cycle_class_size", q)
    end

  fun cycle_power (cycles: int list, k: int) : int list =
    let
      fun expand l =
        let
          val d = gcd (l, k)
          val q = l div d
        in
          List.tabulate (d, fn _ => q)
        end
      val out = List.concat (List.map expand cycles)
    in
      Basic.sort (fn (a, b) => a >= b) out
    end

  fun hook_lengths_edges (edges: bool array) : int list =
    let
      val last = Array.length edges - 1
      fun scan i acc =
        if i > last then
          acc
        else if Array.sub (edges, i) then
          scan (i + 1) acc
        else
          let
            fun scanJ j accJ =
              if j > last then accJ
              else if Array.sub (edges, j) then scanJ (j + 1) ((j - i) :: accJ) else scanJ (j + 1) accJ
          in
            scan (i + 1) (scanJ (i + 1) acc)
          end
    in
      scan 0 []
    end

  fun edge_sequence (lambda: partition) : bool list * int =
    if null lambda orelse List.nth (lambda, 0) = 0 then
      ([], 0)
    else
      let
        val d = List.nth (Partitions.transpose lambda, 0)
        fun mapi f xs =
          let
            fun loop ([], _, acc) = List.rev acc
              | loop (x :: rest, i, acc) = loop (rest, i + 1, f (x, i) :: acc)
          in
            loop (xs, 0, [])
          end
        val vals0 = mapi (fn (part, i) => part - i - 1) lambda
        val vals = Basic.sort (op <=) vals0

        fun mem x =
          (case Basic.binary_search_in (vals, (op <=)) x of
             NONE => false
           | SOME _ => true)

        val w = List.nth (lambda, 0)
        val edges = List.tabulate (w + d, fn t => mem (t - d))
      in
        (edges, d)
      end

  fun countTrueSlice (edges: bool array, lo: int, hi: int) : int =
    let
      fun loop i acc =
        if i >= hi then acc else loop (i + 1) (acc + (if Array.sub (edges, i) then 1 else 0))
    in
      loop lo 0
    end

  fun Sn_representation_dimension_from_edges (edges: bool array) : int =
    let
      val hl = hook_lengths_edges edges
      val n = length hl
      val num = factorialIntInf n
      val denom = List.foldl (fn (h, acc) => acc * IntInf.fromInt h) 1 hl
      val (q, r) = IntInf.divMod (num, denom)
      val () = if r = 0 then () else raise Fail "Combinatorics.Sn_representation_dimension_from_edges: inexact"
    in
      toIntChecked ("Sn_representation_dimension_from_edges", q)
    end

  fun Murnaghan_Nakayama (lambda: partition, cycle_type: int list) : int =
    let
      val () =
        if sumInts lambda = sumInts cycle_type then
          ()
        else
          raise Fail "Combinatorics.Murnaghan_Nakayama: size mismatch"

      val (edgesList, _) = edge_sequence lambda
      val edge = Array.fromList edgesList
      val alpha = Basic.sort (op <=) cycle_type
      val j0 = length alpha - 1
      val lenEdge = Array.length edge

      fun mn j =
        if j < 0 then
          1
        else
          let
            val k = List.nth (alpha, j)
          in
            if k = 1 then
              Sn_representation_dimension_from_edges edge
            else
              let
                val limit = lenEdge - k
                fun loopI i acc =
                  if i >= limit then
                    acc
                  else if (not (Array.sub (edge, i))) andalso Array.sub (edge, i + k) then
                    let
                      val () = Array.update (edge, i + k, false)
                      val () = Array.update (edge, i, true)
                      val sign = if countTrueSlice (edge, i + 1, i + k) mod 2 = 0 then 1 else ~1
                      val term = sign * mn (j - 1)
                      val () = Array.update (edge, i, false)
                      val () = Array.update (edge, i + k, true)
                    in
                      loopI (i + 1) (acc + term)
                    end
                  else
                    loopI (i + 1) acc
              in
                loopI 0 0
              end
          end
    in
      mn j0
    end

  (* ---------------- hyperoctahedral group helpers (ported from `combinatorics.at`) ---------------- *)

  (* Generate pairs of partitions of total size `n`, ordering as in the `.at`
     `pairs_of_total_sum` helper: take larger left size first (i=n..0), and
     within each size use the `lister` ordering. *)
  fun pairs_of_total_sum (n: int, lister: int -> partition list) : bipartition list =
    if n < 0 then
      []
    else
      let
        val lists = List.tabulate (n + 1, lister) (* lists[i] = lister i *)
        fun at i = List.nth (lists, i)
        fun allPairsAt i =
          let
            val lefts = at i
            val rights = at (n - i)
          in
            List.concat (List.map (fn lam => List.map (fn mu => (lam, mu)) rights) lefts)
          end
      in
        List.concat (List.map allPairsAt (List.rev (List.tabulate (n + 1, fn i => i))))
      end

  (* Conjugacy class parameterization for the hyperoctahedral group H_n:
     all bipartitions of total size `n` in the `.at` `partition_pairs` order. *)
  fun partition_pairs (n: int) : bipartition list =
    pairs_of_total_sum (n, Partitions.partitions)

  fun rank_bipartition ((lambda, mu): bipartition) : int = sumInts lambda + sumInts mu

  fun rank_signed_cycles (cy: signed_cycles) : int =
    List.foldl (fn ((l, _), acc) => acc + l) 0 cy

  fun bipartition_toString ((lambda, mu): bipartition) : string =
    "(" ^ Partitions.toString lambda ^ "," ^ Partitions.toString mu ^ ")"

  fun signed_cycles_toString (cy: signed_cycles) : string =
    let
      fun one (l, isNeg) = "(" ^ Int.toString l ^ "," ^ (if isNeg then "-" else "+") ^ ")"
    in
      "[" ^ String.concatWith "," (List.map one cy) ^ "]"
    end

  fun to_cycles ((lambda, mu): bipartition) : signed_cycles =
    List.map (fn part => (part, false)) lambda @ List.map (fn part => (part, true)) mu

  fun signed_cycle_type_order (cycles: signed_cycles) : int =
    let
      fun one ((cycle, sign): signed_cycle) = if sign then cycle + cycle else cycle
    in
      lcm_list (List.map one cycles)
    end

  fun signed_cycle_centralizer_order_IntInf (cycles: signed_cycles) : IntInf.int =
    let
      val plus_cycles = List.map #1 (List.filter (fn (_, s) => not s) cycles)
      val minus_cycles = List.map #1 (List.filter (fn (_, s) => s) cycles)
      val fp = frequencies (Basic.sort (op >=) plus_cycles)
      val fm = frequencies (Basic.sort (op >=) minus_cycles)
      val part0 = productIntInf (List.map (fn (cycle, _) => cycle + cycle) cycles)
      val part1 = List.foldl (fn (m, acc) => acc * factorialIntInf m) 1 fp
      val part2 = List.foldl (fn (m, acc) => acc * factorialIntInf m) 1 fm
    in
      part0 * part1 * part2
    end

  fun signed_cycle_class_size (cycles: signed_cycles) : int =
    let
      val n = rank_signed_cycles cycles
      val num = factorialIntInf n * IntInf.pow (2, n)
      val denom = signed_cycle_centralizer_order_IntInf cycles
      val (q, r) = IntInf.divMod (num, denom)
      val () = if r = 0 then () else raise Fail "Combinatorics.signed_cycle_class_size: inexact"
    in
      toIntChecked ("signed_cycle_class_size", q)
    end

  fun signed_cycle_power (cycles: signed_cycles, k: int) : signed_cycles =
    let
      val m = List.foldl Int.max 0 (List.map #1 cycles)
      val counts = Array.tabulate (m + 1, fn _ => (0, 0)) (* (pos,neg) by length *)

      fun addCount (len: int, isNeg: bool, delta: int) =
        let
          val (p, n) = Array.sub (counts, len)
        in
          if isNeg then Array.update (counts, len, (p, n + delta))
          else Array.update (counts, len, (p + delta, n))
        end

      val () =
        List.app
          (fn (l, sign) =>
             let
               val d = gcd (l, k)
               val len = l div d
               val isNeg = sign andalso (((k div d) mod 2) = 1)
             in
               addCount (len, isNeg, d)
             end)
          cycles

      fun emit (len: int, (pos, neg), accPos, accNeg) =
        ( List.tabulate (pos, fn _ => (len, false)) @ accPos
        , List.tabulate (neg, fn _ => (len, true)) @ accNeg
        )

      val (posAcc, negAcc) =
        List.foldr
          (fn (len, (accP, accN)) =>
             let
               val c = Array.sub (counts, len)
             in
               emit (len, c, accP, accN)
             end)
          ([], [])
          (List.tabulate (m, fn i => i + 1))
    in
      (* `.at` orders negative cycles before positive; within each, larger lengths first. *)
      Basic.sort (fn ((a, _), (b, _)) => a >= b) negAcc @ Basic.sort (fn ((a, _), (b, _)) => a >= b) posAcc
    end

  (* Hyperoctahedral character recursion. *)
  fun hyperoctahedral_character (pair: bipartition, signed_cycle_type: signed_cycles) : int =
    let
      val () =
        if rank_bipartition pair = rank_signed_cycles signed_cycle_type then
          ()
        else
          raise Fail "Combinatorics.hyperoctahedral_character: size mismatch"

      val alpha = Basic.sort_by (#1, (op <=)) signed_cycle_type (* increasing by length *)

      val c = List.length (List.filter (fn (l, s) => l = 1 andalso s) alpha) (* negative 1-cycles *)
      val d = List.length (List.filter (fn (l, s) => l = 1 andalso not s) alpha) (* positive 1-cycles *)

      val (lambda, mu) = pair
      val (edge0List, _) = edge_sequence lambda
      val (edge1List, _) = edge_sequence mu
      val edge0 = Array.fromList edge0List
      val edge1 = Array.fromList edge1List

      fun productHooks (hs: int list) : IntInf.int =
        List.foldl (fn (h, acc) => acc * IntInf.fromInt h) 1 hs

      fun binomI (n: int, k: int) : IntInf.int = binomIntInf (n, k)

      fun baseCase () : int =
        let
          val hl0 = hook_lengths_edges edge0
          val hl1 = hook_lengths_edges edge1
          val a = length hl0
          val b = length hl1
          val () = if a + b = c + d then () else raise Fail "Combinatorics.hyperoctahedral_character: basecase size mismatch"
          val lwb = Int.max (0, d - a)
          val upb = Int.min (b, d)
          fun sumL l acc =
            if l > upb then acc
            else
              let
                val sgn = if l mod 2 = 0 then 1 else ~1
                val term = IntInf.fromInt sgn * binomI (c, b - l) * binomI (d, l)
              in
                sumL (l + 1) (acc + term)
              end
          val comb = sumL lwb 0
          val dim0 =
            let
              val num = factorialIntInf a
              val den = productHooks hl0
              val (q, r) = IntInf.divMod (num, den)
              val () = if r = 0 then () else raise Fail "Combinatorics.hyperoctahedral_character: dim0 inexact"
            in
              q
            end
          val dim1 =
            let
              val num = factorialIntInf b
              val den = productHooks hl1
              val (q, r) = IntInf.divMod (num, den)
              val () = if r = 0 then () else raise Fail "Combinatorics.hyperoctahedral_character: dim1 inexact"
            in
              q
            end
          val out = comb * dim0 * dim1
        in
          toIntChecked ("hyperoctahedral_character", out)
        end

      fun sumHooks (edge: bool array, k: int, recVal: unit -> int) : int =
        let
          val lenEdge = Array.length edge
          val limit = lenEdge - k
          fun loopI i acc =
            if i >= limit then
              acc
            else if (not (Array.sub (edge, i))) andalso Array.sub (edge, i + k) then
              let
                val () = Array.update (edge, i + k, false)
                val () = Array.update (edge, i, true)
                val sign = if countTrueSlice (edge, i + 1, i + k) mod 2 = 0 then 1 else ~1
                val term = sign * recVal ()
                val () = Array.update (edge, i, false)
                val () = Array.update (edge, i + k, true)
              in
                loopI (i + 1) (acc + term)
              end
            else
              loopI (i + 1) acc
        in
          loopI 0 0
        end

      fun f j =
        if j < 0 then
          1
        else
          let
            val (k, sign) = List.nth (alpha, j)
          in
            if k = 1 then
              baseCase ()
            else
              let
                fun recVal () = f (j - 1)
                val s0 = sumHooks (edge0, k, recVal)
                val s1 = sumHooks (edge1, k, recVal)
              in
                if sign then s0 - s1 else s0 + s1
              end
          end
    in
      f (length alpha - 1)
    end

  (* ---------------- Symbol / make_special machinery (ported from `combinatorics.at`) ---------------- *)

  (* Normalize a symbol so that |f| = |g| + 1 and at most one initial 0 occurs. *)
  fun normalize_symbol ((f0, g0): symbol) : symbol =
    let
      fun normalize_row (v: int list) : int list =
        let
          val nv = length v
          fun findI i =
            if i >= nv then nv
            else if List.nth (v, i) <= i then findI (i + 1) else i
          val i0 = findI 0
          fun dropI i xs = List.drop (xs, i)
        in
          List.map (fn e => e - i0) (dropI i0 v)
        end

      fun expand (v: int list, r: int) : int list =
        if r <= 0 then v else List.tabulate (r, fn i => i) @ List.map (fn c => c + r) v

      val f = normalize_row f0
      val g = normalize_row g0
      val d = length f - (length g + 1)
    in
      if d = 0 then (f, g)
      else if d < 0 then (expand (f, ~d), g)
      else (f, expand (g, d))
    end

  (* Convert a bipartition to a normalized symbol. *)
  fun symbol_of_bipartition ((lambda0, mu0): bipartition) : symbol =
    let
      val lambda = strip_to_partition lambda0
      val mu = strip_to_partition mu0
      val d = length lambda - (length mu + 1)
      val lambda' = if d < 0 then lambda @ List.tabulate (~d, fn _ => 0) else lambda
      val mu' = if d > 0 then mu @ List.tabulate (d, fn _ => 0) else mu

      fun revIndex xs i = List.nth (xs, length xs - 1 - i)
      fun rowFrom (p: int list) : int list =
        let
          val l = length p
        in
          List.tabulate (l, fn i => revIndex p i + i)
        end
    in
      normalize_symbol (rowFrom lambda', rowFrom mu')
    end

  (* Convert a normalized symbol back to a bipartition. *)
  fun symbol_to_bipartition ((f, g): symbol) : bipartition =
    let
      fun rowToPartition (row: int list) : partition =
        let
          val l = length row
          val revParts = List.tabulate (l, fn i => List.nth (row, i) - i)
        in
          strip_to_partition (List.rev revParts)
        end
    in
      (rowToPartition f, rowToPartition g)
    end

  fun is_special_symbol ((s0, s1): symbol) : bool =
    let
      val l = length s1
      fun ok i =
        List.nth (s0, i) <= List.nth (s1, i) andalso List.nth (s1, i) <= List.nth (s0, i + 1)
    in
      List.all ok (List.tabulate (l, fn i => i))
    end

  (* Mutating insertion-sort style algorithm from the `.at` `make_special(Symbol)`. *)
  fun make_special_symbol (sym0: symbol) : symbol =
    let
      val (s0List, s1List) = sym0
      val l = length s1List
      val () = if length s0List = l + 1 then () else raise Fail "Combinatorics.make_special_symbol: bad symbol lengths"
      val s0 = Array.fromList s0List
      val s1 = Array.fromList s1List

      fun loopI i =
        if i >= l then
          ()
        else
          let
            val e0 = Array.sub (s0, i)
            fun advance1 j = if j >= l then j else if e0 > Array.sub (s1, j) then advance1 (j + 1) else j
            val j1 = advance1 i
            val () =
              if j1 > i andalso j1 <= l then
                let
                  val tmp = Array.sub (s1, i)
                  val () = Array.update (s0, i, tmp)
                  fun shift k =
                    if k >= j1 - 1 then ()
                    else (Array.update (s1, k, Array.sub (s1, k + 1)); shift (k + 1))
                  val () = shift i
                  val () = Array.update (s1, j1 - 1, e0)
                in
                  ()
                end
              else
                ()

            val e1 = Array.sub (s1, i)
            fun advance0 j = if j >= l + 1 then j else if e1 > Array.sub (s0, j) then advance0 (j + 1) else j
            val j0 = advance0 (i + 1)
            val () =
              if j0 > i + 1 andalso j0 <= l + 1 then
                let
                  val tmp = Array.sub (s0, i + 1)
                  val () = Array.update (s1, i, tmp)
                  fun shift k =
                    if k >= j0 - 1 then ()
                    else (Array.update (s0, k, Array.sub (s0, k + 1)); shift (k + 1))
                  val () = shift (i + 1)
                  val () = Array.update (s0, j0 - 1, e1)
                in
                  ()
                end
              else
                ()
          in
            loopI (i + 1)
          end
    in
      loopI 0;
      (Array.foldr (op ::) [] s0, Array.foldr (op ::) [] s1)
    end

  fun make_special_bipartition (bip: bipartition) : bipartition =
    symbol_to_bipartition (make_special_symbol (symbol_of_bipartition bip))

  (* ---------------- type D (even signed permutations) helpers (ported from `combinatorics.at`) ---------------- *)

  fun is_very_even (p: partition) : bool = List.all (fn x => x mod 2 = 0) p

  fun D_rank_class (c: D_class) : int =
    (case c of
       D_unsplit_class cycles => rank_signed_cycles cycles
     | D_split_class (lambda, _) => sumInts lambda)

  fun D_rank_irrep (chi: D_irrep) : int =
    (case chi of
       D_unsplit_irr (lambda, mu) => sumInts lambda + sumInts mu
     | D_split_irr (lambda, _) => 2 * sumInts lambda)

  fun D_cycle_type_order (c: D_class) : int =
    (case c of
       D_unsplit_class cycles => signed_cycle_type_order cycles
     | D_split_class (lambda, _) => cycle_type_order lambda)

  fun D_centralizer_order_IntInf (c: D_class) : IntInf.int =
    (case c of
       D_unsplit_class cycles =>
         let
           val den = signed_cycle_centralizer_order_IntInf cycles
           val (q, r) = IntInf.divMod (den, 2)
           val () = if r = 0 then () else raise Fail "Combinatorics.D_centralizer_order_IntInf: odd centralizer (unexpected)"
         in
           q
         end
     | D_split_class (alpha, _) =>
         cycle_centralizer_order_IntInf alpha * IntInf.pow (2, length alpha))

  fun D_class_size (c: D_class) : int =
    let
      val n = D_rank_class c
      val () = if n >= 2 then () else raise Fail "Combinatorics.D_class_size: n<2 not supported"
      val num = factorialIntInf n * IntInf.pow (2, n - 1)
      val den = D_centralizer_order_IntInf c
      val (q, r) = IntInf.divMod (num, den)
      val () = if r = 0 then () else raise Fail "Combinatorics.D_class_size: inexact"
    in
      toIntChecked ("D_class_size", q)
    end

  fun D_class_toString (c: D_class) : string =
    (case c of
       D_unsplit_class cycles => signed_cycles_toString cycles
     | D_split_class (alpha, eps) => Partitions.toString alpha ^ (if eps then "-" else "+"))

  fun D_irrep_toString (chi: D_irrep) : string =
    (case chi of
       D_unsplit_irr (mu, lambda) =>
         "{ " ^ Partitions.toString mu ^ " | " ^ Partitions.toString lambda ^ " }"
     | D_split_irr (mu, s) =>
         "{ " ^ Partitions.toString mu ^ " | " ^ Partitions.toString mu ^ " }" ^ (if s then "-" else "+"))

  fun D_classes (n: int) : D_class list =
    if n < 2 then
      raise Fail "Combinatorics.D_classes: n<2 not supported"
    else
      List.concat
        (List.map
           (fn (lambda, mu) =>
              if length mu mod 2 = 1 then
                []
              else if (null mu) andalso is_very_even lambda then
                [D_split_class (lambda, false), D_split_class (lambda, true)]
              else
                [D_unsplit_class (to_cycles (lambda, mu))])
           (partition_pairs n))

  fun D_irreps (n: int) : D_irrep list =
    if n < 2 then
      raise Fail "Combinatorics.D_irreps: n<2 not supported"
    else
      let
        val pairs = pairs_of_total_sum (n, (fn k => List.rev (Partitions.partitions k)))
      in
        List.concat
          (List.map
             (fn (lambda, mu) =>
                (case slex_cmp_partitions (lambda, mu) of
                   ~1 => []
                 | 0 => [D_split_irr (lambda, false), D_split_irr (lambda, true)]
                 | _ => [D_unsplit_irr (lambda, mu)]))
             pairs)
      end

  fun D_character (chi: D_irrep, c: D_class) : int =
    let
      val () =
        if D_rank_irrep chi = D_rank_class c then
          ()
        else
          raise Fail "Combinatorics.D_character: size mismatch"

      fun pos_cycles (mu: partition) : signed_cycles = List.map (fn l => (l, false)) mu
      fun div2_partition (alpha: partition) : partition =
        if is_very_even alpha then List.map (fn x => x div 2) alpha
        else raise Fail "Combinatorics.D_character: expected very even partition"

      fun divExact2 (where': string, x: IntInf.int) : IntInf.int =
        let
          val (q, r) = IntInf.divMod (x, 2)
        in
          if r = 0 then q else raise Fail ("Combinatorics.D_character: " ^ where' ^ " odd")
        end
    in
      case chi of
        D_unsplit_irr pair =>
          (case c of
             D_unsplit_class cycles => hyperoctahedral_character (pair, cycles)
           | D_split_class (alpha, _) => hyperoctahedral_character (pair, pos_cycles alpha))
      | D_split_irr (lambda, epsilon) =>
          (case c of
             D_unsplit_class cycles =>
               let
                 val h = IntInf.fromInt (hyperoctahedral_character ((lambda, lambda), cycles))
                 val q = divExact2 ("unsplit", h)
               in
                 toIntChecked ("D_character", q)
               end
           | D_split_class (alpha, delta) =>
               let
                 val hn = IntInf.fromInt (hyperoctahedral_character ((lambda, lambda), pos_cycles alpha))
                 val sn = IntInf.fromInt (Murnaghan_Nakayama (lambda, div2_partition alpha))
                 val sgn = if epsilon <> delta then ~1 else 1
                 val term = IntInf.fromInt sgn * IntInf.pow (2, length alpha) * sn
                 val q = divExact2 ("split", hn + term)
               in
                 toIntChecked ("D_character", q)
               end)
    end

  fun make_special_D_irrep (chi: D_irrep) : D_irrep =
    (case chi of
       D_split_irr _ => chi
     | D_unsplit_irr (lambda0, mu0) =>
         let
           val lambda = strip_to_partition lambda0
           val mu = strip_to_partition mu0
           val d = length lambda - length mu - 1
           val lambda' = if d < 0 then lambda @ List.tabulate (~d, fn _ => 0) else lambda
           val mu' = if d > 0 then mu @ List.tabulate (d, fn _ => 0) else mu

           fun mapi f xs =
             let
               fun loop ([], _, acc) = List.rev acc
                 | loop (x :: rest, i, acc) = loop (rest, i + 1, f (x, i) :: acc)
             in
               loop (xs, 0, [])
             end

           (* Port of `for e@i in lambda do e-i ~od`: compute `e-i` over the
              partition in forward order, then reverse the resulting list. *)
           fun rowD (p: int list) : int list = List.rev (mapi (fn (e, i) => e - i) p)

           val sym = (rowD lambda', rowD mu')
           val (s0, s1) = make_special_symbol sym

           fun revIndex xs i = List.nth (xs, length xs - 1 - i)
           fun fromSpec (row: int list) : partition =
             let
               val l = length row
               val parts = List.tabulate (l, fn i => revIndex row i + i)
             in
               strip_to_partition parts
             end

           val q0 = fromSpec s0
           val q1 = fromSpec s1
           val (p0, p1) =
             if q0 = q1 then
               (q0, q1)
             else
               (case slex_cmp_partitions (q0, q1) of
                  ~1 => (q1, q0)
                | _ => (q0, q1))
         in
           if p0 = p1 then D_split_irr (p0, false) else D_unsplit_irr (p0, p1)
         end)
end
