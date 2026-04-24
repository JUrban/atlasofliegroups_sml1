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
end
