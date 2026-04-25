(*
  Dependencies
  - These are loaded explicitly so this file can be checked standalone via
    `poly -q < atlas-scripts-sml/springer_table_D.sml`.
*)
use "atlas-scripts-sml/basic.sml";
use "atlas-scripts-sml/partitions.sml";
use "atlas-scripts-sml/combinatorics.sml";

(*
  File: atlas-scripts-sml/springer_table_D.sml

  Purpose
  - Partial SML port of `atlas-scripts/springer_table_D.at` (Springer theory
    in classical type D).

  What this file provides today (pure combinatorics)
  - A diagram-level reconstruction of the Jordan type and the split-orbit
    boolean in type D:
      `diagram_D_to_partition : int list -> partition * bool`
  - The Springer correspondence at the partition level:
      `springer_D : partition * bool -> Combinatorics.D_irrep`
    and a diagram-level convenience wrapper:
      `springer_D_from_diagram : int list -> Combinatorics.D_irrep`
  - Partition-level duality transformation used in type D:
      `D_dual : partition * bool -> partition * bool`
    (the nilpotent-orbit / root-datum layer is not yet ported).

  Status
  - The Atlas-facing `ComplexNilpotent` layer (`nilpotent_orbit_D`,
    `dual_map_D`, and `springer_table_D(rd)`) is not yet ported because the
    current Poly/ML FFI does not expose nilpotent orbits or stratified diagrams.
*)

structure Springer_table_D = struct
  structure B = Basic
  structure Cb = Combinatorics
  structure P = Partitions

  type partition = P.partition
  type diagram = int list
  type D_irrep = Cb.D_irrep

  fun fail msg = raise Fail ("Springer_table_D: " ^ msg)

  fun sumInts (xs: int list) : int = List.foldl (op +) 0 xs

  fun mapi f xs =
    let
      fun loop ([], _, acc) = List.rev acc
        | loop (x :: rest, i, acc) = loop (rest, i + 1, f (x, i) :: acc)
    in
      loop (xs, 0, [])
    end

  fun update_first (xs: 'a list, y: 'a) : 'a list =
    (case xs of
       [] => fail "update_first: empty list"
     | _ :: rest => y :: rest)

  (* Histogram-style frequencies:
       freq[i] = multiplicity of value i in the list.
     (Unlike `Combinatorics.frequencies`, which is run-length encoding on a
      sorted list.) *)
  fun value_frequencies (xs: int list) : int list =
    let
      val maxVal = List.foldl (fn (x, acc) => Int.max (x, acc)) 0 xs
      val () = if List.exists (fn x => x < 0) xs then fail "value_frequencies: negative value" else ()
      val a = Array.array (maxVal + 1, 0)
      val () = List.app (fn x => Array.update (a, x, Array.sub (a, x) + 1)) xs
    in
      Array.foldr (op ::) [] a
    end

  fun partition_frequencies (p: partition) : int list = value_frequencies p

  fun is_doubly_even (p0: partition) : bool =
    let
      val p = Cb.strip_to_partition p0
      val freq = partition_frequencies p
      val n = length freq
      fun freqAt i = if i < 0 orelse i >= n then 0 else List.nth (freq, i)
      fun ok i =
        if i mod 2 = 1 then freqAt i = 0 else freqAt i mod 2 = 0
    in
      List.all ok (List.tabulate (n, fn i => i))
    end

  fun is_valid_D (p0: partition) : bool =
    let
      val p = Cb.strip_to_partition p0
      val freq = partition_frequencies p
      val n = length freq
      fun freqAt i = if i < 0 orelse i >= n then 0 else List.nth (freq, i)
      fun evenOk i = if i mod 2 = 0 then freqAt i mod 2 = 0 else true
    in
      sumInts p mod 2 = 0 andalso List.all evenOk (List.tabulate (n, fn i => i))
    end

  val rlex_cmp_partitions = Cb.rlex_cmp_partitions
  val slex_cmp_partitions = Cb.slex_cmp_partitions

  fun order_pair (a: partition, b: partition) : partition * partition =
    (case slex_cmp_partitions (a, b) of
       ~1 => (b, a)
     | _ => (a, b))

  (* ---------------- partitions_D (pure generator) ---------------- *)

  (* Port of `parity_restricted_partitions(false)` from `combinatorics.at`:
     partitions of `n` where every even part has even multiplicity.

     Note: this implementation is a simple filter over all partitions, rather
     than the dynamic-programming construction used by the `.at` file. *)
  fun parity_restricted_partitions_even_parts (n: int) : partition list =
    if n < 0 then []
    else
      let
        fun ok (p: partition) : bool =
          let
            val freq = partition_frequencies p
            val m = length freq
            fun freqAt i = if i < 0 orelse i >= m then 0 else List.nth (freq, i)
            fun evenOk i = if i mod 2 = 0 then freqAt i mod 2 = 0 else true
          in
            List.all evenOk (List.tabulate (m, fn i => i))
          end
      in
        List.filter ok (P.partitions n)
      end

  (* Partition/flip pairs for D_n: partitions of 2n with even parts even
     multiplicities; additionally, if all parts are even, the orbit splits and
     we return both flip choices. *)
  fun partitions_D (n: int) : (partition * bool) list =
    let
      val parts = Cb.parity_restricted_partitions false (2 * n)
      fun all_even p = List.all (fn x => x mod 2 = 0) p
      fun expand p = if all_even p then [(p, false), (p, true)] else [(p, false)]
    in
      List.concat (List.map expand parts)
    end

  (* ---------------- diagram -> partition (ported from `.at`) ---------------- *)

  fun diagram_D_to_partition (diagram: diagram) : partition * bool =
    let
      val (d0, d1) =
        (case diagram of
           a :: b :: _ => (a, b)
         | _ => fail "diagram_D_to_partition: diagram too short")
      val diff = d0 - d1
      val q = diff div 2
      val r = diff mod 2
      val () = if r = 0 then () else fail "diagram_D_to_partition: odd branch sum"

      val diagram' = update_first (diagram, q)
      val hSO0 = B.cumulate_backward diagram'
      val flip = q < 0
      val hSO =
        if flip then
          (case hSO0 of
             [] => []
           | x :: xs => (~x) :: xs)
        else
          hSO0

      val freq0 = value_frequencies hSO
      val max = length freq0
      val freq =
        (case freq0 of
           [] => []
         | z :: zs => (2 * z) :: zs) (* `freq_H[0] *:= 2` in `.at` *)

      fun freqAt i = if i < 0 orelse i >= max then 0 else List.nth (freq, i)

      fun mult i =
        let
          val m = if i + 1 < max then freqAt (i - 1) - freqAt (i + 1) else freqAt (i - 1)
        in
          if m < 0 then fail ("diagram_D_to_partition: negative multiplicity of part " ^ Int.toString i) else m
        end

      fun repeat (0, _, acc) = acc
        | repeat (k, i, acc) = repeat (k - 1, i, i :: acc)

      fun loop (i, acc) =
        if i < 1 then acc else loop (i - 1, repeat (mult i, i, acc))
    in
      (loop (max, []), flip)
    end

  (* ---------------- Springer correspondence on partitions ---------------- *)

  fun springer_D (jordan0: partition, flip: bool) : D_irrep =
    let
      val jordan = Cb.strip_to_partition jordan0
      val () = if is_valid_D jordan then () else fail "springer_D: invalid type D Jordan type"
      val (_, (lambda0, mu0)) = Cb.core_quotient_2 jordan
      val (lambda, mu) = order_pair (lambda0, mu0)
    in
      if lambda = mu then
        Cb.D_split_irr (lambda, (flip <> (sumInts lambda mod 2 = 1)))
      else if flip then
        fail "springer_D: flip with unequal quotient partitions"
      else
        Cb.D_unsplit_irr (lambda, mu)
    end

  fun springer_D_from_diagram (diagram: diagram) : D_irrep =
    let
      val (p, flip) = diagram_D_to_partition diagram
    in
      springer_D (p, flip)
    end

  (* ---------------- duality on partitions ---------------- *)

  fun last_index (limit: int, pred: int -> bool) : int =
    let
      fun loop i =
        if i < 0 then ~1 else if pred i then i else loop (i - 1)
    in
      loop (limit - 1)
    end

  fun partition_frequencies_slack (slack: int) (p: partition) : int list =
    if slack < 0 then fail "partition_frequencies_slack: slack<0"
    else partition_frequencies p @ List.tabulate (slack, fn _ => 0)

  fun repeat_parts (freq: int list) : partition =
    let
      val n = length freq
      fun rep (0, _, acc) = acc
        | rep (k, i, acc) = rep (k - 1, i, i :: acc)
      fun loop (i, acc) =
        if i <= 0 then acc
        else loop (i - 1, rep (List.nth (freq, i), i, acc))
    in
      loop (n - 1, [])
    end

  (* `.at` `D_adjust`: identical algorithm to `B_adjust`, but for even sum. *)
  fun D_adjust (p0: partition) : partition =
    let
      val p = Cb.strip_to_partition p0
      val () = if sumInts p mod 2 = 0 then () else fail "D_adjust: partition sum is not even"
      val freq0 = partition_frequencies_slack 2 p
      val freq = Array.fromList freq0
      val () = if Array.length freq = 0 then () else Array.update (freq, 0, 1)

      fun freqAt i = Array.sub (freq, i)
      fun setAt (i, v) = Array.update (freq, i, v)
      fun decAt i = setAt (i, freqAt i - 1)
      fun incAt i = setAt (i, freqAt i + 1)

      val n = Array.length freq
      val l0 = (n + 1) div 2

      fun evenMultiplicity i = freqAt (2 * i) mod 2 = 1
      fun evenOccurs i = freqAt (2 * i) > 0

      fun loop (limit: int) : unit =
        let
          val l = last_index (limit, evenMultiplicity)
        in
          if l <= 0 then
            ()
          else
            let
              val k = last_index (l, evenOccurs)
              val () = if k >= 0 then () else fail "D_adjust: no smaller even part found"
              val L = 2 * l
              val K = 2 * k
              val () = decAt (L)
              val () = incAt (L - 1)
              val () = incAt (K + 1)
              val () = decAt (K)
            in
              loop (k + 1)
            end
        end
    in
      loop l0;
      repeat_parts (Array.foldr (op ::) [] freq)
    end

  fun D_dual (jordan0: partition, flip: bool) : partition * bool =
    let
      val jordan = Cb.strip_to_partition jordan0
    in
      if is_doubly_even jordan then
        (P.transpose jordan, (flip <> (((sumInts jordan) div 4) mod 2 = 1)))
      else
        (D_adjust (P.transpose jordan), false)
    end

  (* TODO
     - Port the Atlas-facing layer (`nilpotent_orbit_D`, `dual_map_D`, and
       `springer_table_D(rd)`), once nilpotent-orbit/diagram access exists in
       `AtlasFFI`. *)
end
