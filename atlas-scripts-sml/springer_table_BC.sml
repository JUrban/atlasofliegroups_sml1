(*
  Dependencies
  - These are loaded explicitly so this file can be checked standalone via
    `poly -q < atlas-scripts-sml/springer_table_BC.sml`.
*)
use "atlas-scripts-sml/basic.sml";
use "atlas-scripts-sml/partitions.sml";
use "atlas-scripts-sml/combinatorics.sml";

(*
  File: atlas-scripts-sml/springer_table_BC.sml

  Purpose
  - Partial SML port of `atlas-scripts/springer_table_BC.at` (Springer theory
    in classical types B/C).

  What this file provides today
  - Pure combinatorial translations that do not require the Atlas interpreter:
    - `diagram_C_to_partition` and `diagram_B_to_partition`:
        stratified-diagram vectors -> Jordan partitions.
    - `springer_C_from_diagram` and `springer_B_from_diagram`:
        diagram -> bipartition (Weyl-group irrep parameter).
    - `inverse_springer_partition_{B,C}` (validity check + reconstruction)
      using only the parity constraints for types B/C.

  What is still missing
  - The Atlas-level `ComplexNilpotent` layer (`nilpotent_orbit_*`,
    `partition_of_orbit_*`, duality maps, and the packaging into a full
    `SpringerTable` as in `.at`).

  Status
  - Incremental port in progress; the functions above are intended to be
    drop-in building blocks for the remaining translations.
*)

structure Springer_table_BC = struct
  structure B = Basic
  structure Cb = Combinatorics

  type partition = Partitions.partition
  type bipartition = partition * partition
  type diagram = int list

  fun fail msg = raise Fail ("Springer_table_BC: " ^ msg)

  fun mapi f xs =
    let
      fun loop ([], _, acc) = List.rev acc
        | loop (x :: rest, i, acc) = loop (rest, i + 1, f (x, i) :: acc)
    in
      loop (xs, 0, [])
    end

  fun update_last (xs: 'a list, y: 'a) : 'a list =
    if null xs then fail "update_last: empty list"
    else
      let
        val n = length xs
      in
        List.take (xs, n - 1) @ [y]
      end

  (* Histogram-style frequencies:
       freq[i] = multiplicity of part size i in the partition/list. *)
  fun value_frequencies (xs: int list) : int list =
    let
      val maxVal = List.foldl (fn (x, acc) => Int.max (x, acc)) 0 xs
      val () = if maxVal < 0 then fail "value_frequencies: negative value" else ()
      val a = Array.array (maxVal + 1, 0)
      val () =
        List.app
          (fn x =>
             if x < 0 then fail "value_frequencies: negative value"
             else Array.update (a, x, Array.sub (a, x) + 1))
          xs
    in
      Array.foldr (op ::) [] a
    end

  fun partition_frequencies (p: partition) : int list = value_frequencies p

  (* Like `partition_frequencies`, but pads with `slack` trailing zeros so
     in-place frequency-vector algorithms (e.g. `B_adjust`, `C_adjust`) may
     safely increment multiplicities at new maximal part sizes. *)
  fun partition_frequencies_slack (slack: int) (p: partition) : int list =
    if slack < 0 then fail "partition_frequencies_slack: slack<0"
    else
      let
        val freq = partition_frequencies p
      in
        freq @ List.tabulate (slack, fn _ => 0)
      end

  fun sumInts (xs: int list) : int = List.foldl (op +) 0 xs

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

  fun last_index (limit: int, pred: int -> bool) : int =
    let
      fun loop i =
        if i < 0 then ~1 else if pred i then i else loop (i - 1)
    in
      loop (limit - 1)
    end

  fun is_valid_C (lambda: partition) : bool =
    let
      val freq = partition_frequencies lambda
      fun ok i =
        if i mod 2 = 1 then List.nth (freq, i) mod 2 = 0 else true
        handle Subscript => true
    in
      List.all ok (List.tabulate (length freq, fn i => i))
    end

  fun is_valid_B (lambda: partition) : bool =
    let
      val freq = partition_frequencies lambda
      fun evenOk i =
        if i mod 2 = 0 then List.nth (freq, i) mod 2 = 0 else true
        handle Subscript => true
      fun oddSum i acc =
        if i >= length freq then acc
        else if i mod 2 = 1 then oddSum (i + 1) (acc + List.nth (freq, i)) else oddSum (i + 1) acc
      val oddCount = oddSum 0 0
    in
      List.all evenOk (List.tabulate (length freq, fn i => i)) andalso oddCount mod 2 = 1
    end

  (* ---------------- duality adjustments on partitions (ported from `.at`) ---------------- *)

  (* `.at` `C_adjust`: make multiplicities of odd parts even. *)
  fun C_adjust (p0: partition) : partition =
    let
      val p = Cb.strip_to_partition p0
      val () = if sumInts p mod 2 = 0 then () else fail "C_adjust: partition sum is not even"
      val freq0 = partition_frequencies_slack 2 p
      val freq = Array.fromList freq0

      fun freqAt i = Array.sub (freq, i)
      fun setAt (i, v) = Array.update (freq, i, v)
      fun decAt i = setAt (i, freqAt i - 1)
      fun incAt i = setAt (i, freqAt i + 1)

      fun oddMultiplicity i = freqAt (2 * i + 1) mod 2 = 1
      fun oddOccurs i = freqAt (2 * i + 1) > 0

      val n = Array.length freq
      val l0 = n div 2

      fun loop (limit: int) : unit =
        let
          val l = last_index (limit, oddMultiplicity)
        in
          if l < 0 then
            ()
          else
            let
              val () = if l > 0 then () else fail "C_adjust: internal invariant violated (l=0)"
              val k = last_index (l, oddOccurs)
              val () = if k >= 0 then () else fail "C_adjust: no smaller odd part found"
              val L = 2 * l
              val K = 2 * k
              val () = decAt (L + 1)
              val () = incAt (L)
              val () = incAt (K + 2)
              val () = decAt (K + 1)
            in
              loop (k + 1)
            end
        end
    in
      loop l0;
      repeat_parts (Array.foldr (op ::) [] freq)
    end

  (* `.at` `B_adjust`: make multiplicities of nonzero even parts even. *)
  fun B_adjust (p0: partition) : partition =
    let
      val p = Cb.strip_to_partition p0
      val () = if sumInts p mod 2 = 1 then () else fail "B_adjust: partition sum is not odd"
      val freq0 = partition_frequencies_slack 2 p
      val freq = Array.fromList freq0
      val () = if Array.length freq = 0 then () else Array.update (freq, 0, 1) (* seed *)

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
              val () = if k >= 0 then () else fail "B_adjust: no smaller even part found"
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

  (* `.at` `B_to_C_dual` and `C_to_B_dual`, at the partition level. *)
  fun B_to_C_dual (lambda0: partition) : partition =
    let
      val lambda = Cb.strip_to_partition lambda0
      val () = if null lambda then fail "B_to_C_dual: empty partition" else ()
      val lastPart = List.last lambda
      val lambda1 = update_last (lambda, lastPart - 1)
    in
      C_adjust (Partitions.transpose (C_adjust lambda1))
    end

  fun C_to_B_dual (lambda0: partition) : partition =
    let
      val lambda = Cb.strip_to_partition lambda0
      val lambda1 =
        (case lambda of
           [] => [1]
         | x :: xs => (x + 1) :: xs)
    in
      B_adjust (Partitions.transpose (B_adjust lambda1))
    end

  (* ---------------- diagram -> partition (ported from `.at`) ---------------- *)

  (* `.at` `diagram_C_to_partition` *)
  fun diagram_C_to_partition (diagram: diagram) : partition =
    let
      val last =
        (case List.rev diagram of
           [] => fail "diagram_C_to_partition: empty diagram"
         | x :: _ => x)
      val (q, r) = (last div 2, last mod 2)
      val () = if r = 1 then () else fail "diagram_C_to_partition: final diagram entry is not odd"
      val diagram' = update_last (diagram, q)
      val hSp = B.cumulate_backward diagram'

      val freq0 = value_frequencies hSp
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
          if m < 0 then fail ("diagram_C_to_partition: negative multiplicity of part " ^ Int.toString i) else m
        end

      fun repeat (0, _, acc) = acc
        | repeat (k, i, acc) = repeat (k - 1, i, i :: acc)

      fun loop (i, acc) =
        if i < 1 then acc else loop (i - 1, repeat (mult i, i, acc))
    in
      loop (max, [])
    end

  (* `.at` `diagram_B_to_partition` *)
  fun diagram_B_to_partition (diagram: diagram) : partition =
    let
      val hSo = B.cumulate_backward diagram

      val freq0 = value_frequencies hSo
      val max = length freq0
      val freq =
        (case freq0 of
           [] => []
         | z :: zs => (2 * z + 1) :: zs) (* `freq_H[0] +:= freq_H[0]+1` *)

      fun freqAt i = if i < 0 orelse i >= max then 0 else List.nth (freq, i)

      fun mult i =
        let
          val m = if i + 1 < max then freqAt (i - 1) - freqAt (i + 1) else freqAt (i - 1)
        in
          if m < 0 then fail ("diagram_B_to_partition: negative multiplicity of part " ^ Int.toString i) else m
        end

      fun repeat (0, _, acc) = acc
        | repeat (k, i, acc) = repeat (k - 1, i, i :: acc)

      fun loop (i, acc) =
        if i < 1 then acc else loop (i - 1, repeat (mult i, i, acc))
    in
      loop (max, [])
    end

  (* ---------------- Springer map at diagram level ---------------- *)

  (* Diagram-level analogue of `.at` `springer_C` (returns a bipartition). *)
  fun springer_C_from_diagram (diagram: diagram) : bipartition =
    let
      val (_, (mu, lambda)) = Cb.core_quotient_2 (diagram_C_to_partition diagram)
    in
      (lambda, mu)
    end

  (* Diagram-level analogue of `.at` `springer_B` (returns a bipartition). *)
  fun springer_B_from_diagram (diagram: diagram) : bipartition =
    let
      val (_, pair) = Cb.core_quotient_2 (diagram_B_to_partition diagram)
    in
      pair
    end

  (* ---------------- inverse Springer on partitions (ported shape) ---------------- *)

  fun inverse_springer_partition_C (pQ: bipartition) : bool * partition =
    let
      val (p, q) = pQ
      val lambda = Cb.from_core_quotient_2 (0, (q, p))
    in
      (is_valid_C lambda, lambda)
    end

  fun inverse_springer_partition_B (pQ: bipartition) : bool * partition =
    let
      val lambda = Cb.from_core_quotient_2 (0, pQ)
    in
      (is_valid_B lambda, lambda)
    end

  (* TODO
     - Port the Atlas-facing `ComplexNilpotent` layer (`nilpotent_orbit_*`,
       `partition_of_orbit_*`, `dual_map_{B,C}`, and `springer_table_{B,C}`),
       then expose diagram/partition/bipartition mappings through that API. *)
end
