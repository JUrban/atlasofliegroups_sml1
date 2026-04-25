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
     - Port the duality adjustments (`C_adjust`, `B_adjust`, `B_to_C_dual`, ...)
       and the `SpringerTable` packaging from `springer_table_BC.at`. *)
end
