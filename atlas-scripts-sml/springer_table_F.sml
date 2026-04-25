use "atlas-scripts-sml/basic.sml";
use "atlas-scripts-sml/sort.sml";
use "atlas-scripts-sml/springer_tables.sml";

(*
  File: atlas-scripts-sml/springer_table_F.sml

  Purpose
  - Partial SML translation of `atlas-scripts/springer_table_F.at`.
  - The `.at` script defines:
      - a duality map on nilpotent orbit diagrams (as integer vectors), and
      - a “Springer map” from orbit diagrams to character-table indices.

  What is ported here
  - The pure diagram-level lookup tables:
      - `F4_nilpotent_orbit_dual_map : diagram -> diagram`
      - `F4_Springer_map : diagram -> int`
  - These do not depend on the Atlas interpreter’s `ComplexNilpotent` or
    `SpringerTable` types; they operate only on diagrams represented as `int list`.
*)

structure Springer_table_F = struct
  type diagram = int list

  fun leqDiagram (a: diagram, b: diagram) : bool = Sort.rlex_leq (a, b)

  fun sortByKey (xs: (diagram * 'a) list) : (diagram * 'a) list =
    Basic.sort (fn ((a, _), (b, _)) => leqDiagram (a, b)) xs

  fun lookupSorted (table: (diagram * 'a) list, d: diagram, what: string) : 'a =
    let
      val n = length table
      fun keyAt i = #1 (List.nth (table, i))
      fun pred i = leqDiagram (d, keyAt i)
      val k = Basic.binary_search_first (pred, 0, n)
      val () = if k < n then () else raise Fail ("Springer_table_F: " ^ what ^ ": unknown diagram")
      val (src, img) = List.nth (table, k)
      val () = if src = d then () else raise Fail ("Springer_table_F: " ^ what ^ ": unknown diagram")
    in
      img
    end

  val F4_nilpotent_orbit_dual_table_unsorted : (diagram * diagram) list =
    [ ([0, 0, 0, 0], [2, 2, 2, 2])
    , ([1, 0, 0, 0], [2, 0, 2, 2])
    , ([0, 0, 0, 1], [2, 0, 2, 2])
    , ([0, 1, 0, 0], [2, 0, 2, 0])
    , ([2, 0, 0, 0], [0, 0, 2, 2])
    , ([0, 0, 0, 2], [2, 1, 0, 1])
    , ([0, 0, 1, 0], [0, 0, 2, 0])
    , ([2, 0, 0, 1], [0, 0, 2, 0])
    , ([0, 1, 0, 1], [0, 0, 2, 0])
    , ([1, 0, 1, 0], [0, 0, 2, 0])
    , ([0, 2, 0, 0], [0, 0, 2, 0])
    , ([1, 0, 1, 2], [2, 0, 0, 0])
    , ([2, 2, 0, 0], [0, 0, 0, 2])
    , ([0, 2, 0, 2], [0, 0, 1, 0])
    , ([2, 2, 0, 2], [1, 0, 0, 0])
    , ([2, 2, 2, 2], [0, 0, 0, 0])
    ]

  val F4_nilpotent_orbit_dual_table : (diagram * diagram) list =
    sortByKey F4_nilpotent_orbit_dual_table_unsorted

  fun F4_nilpotent_orbit_dual_map (d: diagram) : diagram =
    lookupSorted (F4_nilpotent_orbit_dual_table, d, "dual_map")

  val F4_Springer_table_unsorted : (diagram * int) list =
    [ ([0, 0, 0, 0], 3)
    , ([1, 0, 0, 0], 7)
    , ([0, 0, 0, 1], 19)
    , ([0, 1, 0, 0], 12)
    , ([2, 0, 0, 0], 23)
    , ([0, 0, 0, 2], 21)
    , ([0, 0, 1, 0], 17)
    , ([2, 0, 0, 1], 10)
    , ([0, 1, 0, 1], 13)
    , ([1, 0, 1, 0], 24)
    , ([0, 2, 0, 0], 15)
    , ([1, 0, 1, 2], 22)
    , ([2, 2, 0, 0], 20)
    , ([0, 2, 0, 2], 9)
    , ([2, 2, 0, 2], 16)
    , ([2, 2, 2, 2], 0)
    ]

  val F4_Springer_table : (diagram * int) list = sortByKey F4_Springer_table_unsorted

  fun F4_Springer_map (d: diagram) : int =
    lookupSorted (F4_Springer_table, d, "Springer_map")

  val diagram_table : Springer_tables.diagram_table =
    Springer_tables.make_involutive (F4_nilpotent_orbit_dual_map, F4_Springer_map)
end
