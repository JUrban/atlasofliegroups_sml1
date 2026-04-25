use "atlas-scripts-sml/basic.sml";
use "atlas-scripts-sml/sort.sml";
use "atlas-scripts-sml/springer_tables.sml";

(*
  File: atlas-scripts-sml/springer_table_E6.sml

  Purpose
  - Partial SML translation of `atlas-scripts/springer_table_E6.at`.
  - The `.at` script defines:
      - a duality map on nilpotent orbit diagrams (as integer vectors), and
      - a “Springer map” from orbit diagrams to character-table indices.

  What is ported here
  - The pure diagram-level lookup tables:
      - `E6_nilpotent_orbit_dual_map : diagram -> diagram`
      - `E6_Springer_map : diagram -> int`
  - These do not depend on the Atlas interpreter’s `ComplexNilpotent` or
    `SpringerTable` types; they operate only on diagrams represented as `int list`.
*)

structure Springer_table_E6 = struct
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
      val () = if k < n then () else raise Fail ("Springer_table_E6: " ^ what ^ ": unknown diagram")
      val (src, img) = List.nth (table, k)
      val () = if src = d then () else raise Fail ("Springer_table_E6: " ^ what ^ ": unknown diagram")
    in
      img
    end

  val E6_nilpotent_orbit_dual_table_unsorted : (diagram * diagram) list =
    [ ([0, 0, 0, 0, 0, 0], [2, 2, 2, 2, 2, 2])
    , ([0, 1, 0, 0, 0, 0], [2, 2, 2, 0, 2, 2])
    , ([1, 0, 0, 0, 0, 1], [2, 2, 0, 2, 0, 2])
    , ([0, 0, 0, 1, 0, 0], [2, 0, 0, 2, 0, 2])
    , ([0, 2, 0, 0, 0, 0], [2, 0, 0, 2, 0, 2])
    , ([1, 1, 0, 0, 0, 1], [1, 2, 1, 0, 1, 1])
    , ([2, 0, 0, 0, 0, 2], [0, 2, 0, 2, 0, 0])
    , ([0, 0, 1, 0, 1, 0], [1, 1, 1, 0, 1, 1])
    , ([1, 2, 0, 0, 0, 1], [2, 2, 0, 0, 0, 2])
    , ([1, 0, 0, 1, 0, 1], [0, 0, 0, 2, 0, 0])
    , ([0, 1, 1, 0, 1, 0], [0, 0, 0, 2, 0, 0])
    , ([0, 0, 0, 2, 0, 0], [0, 0, 0, 2, 0, 0])
    , ([2, 2, 0, 0, 0, 2], [1, 2, 0, 0, 0, 1])
    , ([0, 2, 0, 2, 0, 0], [2, 0, 0, 0, 0, 2])
    , ([1, 1, 1, 0, 1, 1], [0, 0, 1, 0, 1, 0])
    , ([2, 1, 1, 0, 1, 2], [0, 2, 0, 0, 0, 0])
    , ([1, 2, 1, 0, 1, 1], [1, 1, 0, 0, 0, 1])
    , ([2, 0, 0, 2, 0, 2], [0, 2, 0, 0, 0, 0])
    , ([2, 2, 0, 2, 0, 2], [1, 0, 0, 0, 0, 1])
    , ([2, 2, 2, 0, 2, 2], [0, 1, 0, 0, 0, 0])
    , ([2, 2, 2, 2, 2, 2], [0, 0, 0, 0, 0, 0])
    ]

  val E6_nilpotent_orbit_dual_table : (diagram * diagram) list =
    sortByKey E6_nilpotent_orbit_dual_table_unsorted

  fun E6_nilpotent_orbit_dual_map (d: diagram) : diagram =
    lookupSorted (E6_nilpotent_orbit_dual_table, d, "dual_map")

  val E6_Springer_table_unsorted : (diagram * int) list =
    [ ([0, 0, 0, 0, 0, 0], 1)
    , ([0, 1, 0, 0, 0, 0], 3)
    , ([1, 0, 0, 0, 0, 1], 10)
    , ([0, 0, 0, 1, 0, 0], 5)
    , ([0, 2, 0, 0, 0, 0], 14)
    , ([1, 1, 0, 0, 0, 1], 20)
    , ([2, 0, 0, 0, 0, 2], 13)
    , ([0, 0, 1, 0, 1, 0], 17)
    , ([1, 2, 0, 0, 0, 1], 23)
    , ([1, 0, 0, 1, 0, 1], 4)
    , ([0, 1, 1, 0, 1, 0], 18)
    , ([0, 0, 0, 2, 0, 0], 21)
    , ([2, 2, 0, 0, 0, 2], 22)
    , ([0, 2, 0, 2, 0, 0], 12)
    , ([1, 1, 1, 0, 1, 1], 16)
    , ([2, 1, 1, 0, 1, 2], 7)
    , ([1, 2, 1, 0, 1, 1], 19)
    , ([2, 0, 0, 2, 0, 2], 15)
    , ([2, 2, 0, 2, 0, 2], 9)
    , ([2, 2, 2, 0, 2, 2], 2)
    , ([2, 2, 2, 2, 2, 2], 0)
    ]

  val E6_Springer_table : (diagram * int) list = sortByKey E6_Springer_table_unsorted

  fun E6_Springer_map (d: diagram) : int =
    lookupSorted (E6_Springer_table, d, "Springer_map")

  val diagram_table : Springer_tables.diagram_table =
    Springer_tables.make_involutive (E6_nilpotent_orbit_dual_map, E6_Springer_map)
end
