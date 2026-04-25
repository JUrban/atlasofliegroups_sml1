use "atlas-scripts-sml/basic.sml";
use "atlas-scripts-sml/partitions.sml";

(*
  File: atlas-scripts-sml/springer_table_A.sml

  Purpose
  - Partial SML translation of `atlas-scripts/springer_table_A.at`.
  - The full `.at` script builds a `SpringerTable` for type A using nilpotent
    orbits and character tables. The SML port does not yet implement those
    layers, but we can still port the key *combinatorial* conversion:
      stratified diagram -> partition.

  What is implemented
  - `diagram_A_to_partition : int list -> Partitions.partition`
    Port of the `.at` function of the same name.

  Notes
  - The `.at` code expects the stratified diagram for type A to be a palindrome;
    we keep that check.
  - The result is a partition of `rank+1` (for type `A_rank`).
*)

structure Springer_table_A = struct
  type diagram = int list
  type partition = Partitions.partition

  fun fail where' msg = raise Fail ("Springer_table_A." ^ where' ^ ": " ^ msg)

  (* Multiplicity vector by value.
     Returns a list `freq` where `freq[i]` is the number of occurrences of `i`
     in `xs` (for `0 <= i <= max(xs)`), and `freq` is always nonempty. *)
  fun value_frequencies (xs: int list) : int list =
    let
      val () = if List.all (fn x => x >= 0) xs then () else fail "value_frequencies" "negative entry"
      val mx = List.foldl Int.max 0 xs
      val a = Array.array (mx + 1, 0)
      val () = List.app (fn x => Array.update (a, x, Array.sub (a, x) + 1)) xs
    in
      List.tabulate (mx + 1, fn i => Array.sub (a, i))
    end

  (* Port of `diagram_A_to_partition` from `springer_table_A.at`. *)
  fun diagram_A_to_partition (diagram: diagram) : partition =
    let
      val () =
        if diagram = List.rev diagram then
          ()
        else
          fail "diagram_A_to_partition" "stratified diagram should be a palindrome"

      val seq = 0 :: Basic.cumulate_forward diagram
      val freq = value_frequencies seq

      val n = length freq
      val q = n div 2
      val r = n mod 2
      val () = if r = 1 then () else fail "diagram_A_to_partition" "diagram has even sum"
      val maxPart = q + 1

      fun freqAt i =
        if i < 0 orelse i >= length freq then 0 else List.nth (freq, i)

      fun mult i =
        let
          val m =
            if i + 2 <= maxPart then
              freqAt (maxPart - i) - freqAt (maxPart - i - 2)
            else
              freqAt (maxPart - i)
        in
          if m < 0 then fail "diagram_A_to_partition" ("negative multiplicity at " ^ Int.toString i) else m
        end

      fun replicate (k, n) = List.tabulate (n, fn _ => k)

      fun loop (i, acc) =
        if i < 1 then acc
        else loop (i - 1, acc @ replicate (i, mult i))
    in
      loop (maxPart, [])
    end
end
