use "atlas-scripts-sml/basic.sml";
use "atlas-scripts-sml/sort.sml";

(*
  File: atlas-scripts-sml/springer_table_E7.sml

  Purpose
  - Partial SML translation of `atlas-scripts/springer_table_E7.at`.
  - The `.at` script defines:
      - a duality map on nilpotent orbit diagrams (as integer vectors), and
      - a “Springer map” from orbit diagrams to character-table indices.

  What is ported here
  - The pure diagram-level lookup tables:
      - `E7_nilpotent_orbit_dual_map : diagram -> diagram`
      - `E7_Springer_map : diagram -> int`
  - These do not depend on the Atlas interpreter’s `ComplexNilpotent` or
    `SpringerTable` types; they operate only on diagrams represented as `int list`.

  Notes
  - The `.at` tables are sorted before binary-search lookup; we mirror that by
    sorting once at initialization.
*)

structure Springer_table_E7 = struct
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
      val () = if k < n then () else raise Fail ("Springer_table_E7: " ^ what ^ ": unknown diagram")
      val (src, img) = List.nth (table, k)
      val () = if src = d then () else raise Fail ("Springer_table_E7: " ^ what ^ ": unknown diagram")
    in
      img
    end

  val E7_nilpotent_orbit_dual_table_unsorted : (diagram * diagram) list =
    [ ([0, 0, 0, 0, 0, 0, 0], [2, 2, 2, 2, 2, 2, 2])
    , ([1, 0, 0, 0, 0, 0, 0], [2, 2, 2, 0, 2, 2, 2])
    , ([0, 0, 0, 0, 0, 1, 0], [2, 2, 2, 0, 2, 0, 2])
    , ([0, 0, 0, 0, 0, 0, 2], [2, 0, 2, 2, 0, 2, 0])
    , ([0, 0, 1, 0, 0, 0, 0], [2, 0, 0, 2, 0, 2, 2])
    , ([2, 0, 0, 0, 0, 0, 0], [2, 0, 0, 2, 0, 2, 2])
    , ([0, 1, 0, 0, 0, 0, 1], [2, 0, 0, 2, 0, 2, 0])
    , ([1, 0, 0, 0, 0, 1, 0], [2, 0, 0, 2, 0, 2, 0])
    , ([0, 0, 0, 1, 0, 0, 0], [2, 0, 0, 2, 0, 0, 2])
    , ([2, 0, 0, 0, 0, 1, 0], [2, 1, 1, 0, 1, 0, 2])
    , ([0, 0, 0, 0, 0, 2, 0], [2, 1, 1, 0, 1, 1, 0])
    , ([0, 2, 0, 0, 0, 0, 0], [0, 0, 0, 2, 0, 2, 0])
    , ([2, 0, 0, 0, 0, 0, 2], [2, 0, 2, 0, 0, 2, 0])
    , ([0, 0, 1, 0, 0, 1, 0], [0, 0, 0, 2, 0, 0, 2])
    , ([1, 0, 0, 1, 0, 0, 0], [0, 0, 0, 2, 0, 0, 2])
    , ([0, 0, 2, 0, 0, 0, 0], [0, 0, 0, 2, 0, 0, 2])
    , ([1, 0, 0, 0, 1, 0, 1], [0, 0, 2, 0, 0, 2, 0])
    , ([2, 0, 2, 0, 0, 0, 0], [2, 0, 0, 0, 0, 2, 2])
    , ([0, 1, 1, 0, 0, 0, 1], [0, 0, 2, 0, 0, 2, 0])
    , ([0, 0, 0, 1, 0, 1, 0], [2, 0, 0, 0, 2, 0, 0])
    , ([2, 0, 0, 0, 0, 2, 0], [2, 0, 0, 1, 0, 1, 0])
    , ([0, 0, 0, 0, 2, 0, 0], [0, 0, 0, 2, 0, 0, 0])
    , ([2, 0, 0, 0, 0, 2, 2], [2, 0, 2, 0, 0, 0, 0])
    , ([2, 1, 1, 0, 0, 0, 1], [2, 0, 0, 0, 0, 2, 0])
    , ([1, 0, 0, 1, 0, 1, 0], [1, 0, 0, 1, 0, 1, 0])
    , ([2, 0, 0, 1, 0, 1, 0], [2, 0, 0, 0, 0, 2, 0])
    , ([0, 0, 0, 2, 0, 0, 0], [0, 0, 0, 0, 2, 0, 0])
    , ([1, 0, 0, 1, 0, 2, 0], [0, 1, 1, 0, 0, 0, 1])
    , ([1, 0, 0, 1, 0, 1, 2], [0, 0, 2, 0, 0, 0, 0])
    , ([2, 0, 0, 0, 2, 0, 0], [0, 0, 0, 1, 0, 1, 0])
    , ([0, 1, 1, 0, 1, 0, 2], [0, 0, 2, 0, 0, 0, 0])
    , ([0, 0, 2, 0, 0, 2, 0], [0, 1, 1, 0, 0, 0, 1])
    , ([2, 0, 2, 0, 0, 2, 0], [2, 0, 0, 0, 0, 0, 2])
    , ([0, 0, 0, 2, 0, 0, 2], [0, 0, 2, 0, 0, 0, 0])
    , ([0, 0, 0, 2, 0, 2, 0], [0, 2, 0, 0, 0, 0, 0])
    , ([2, 1, 1, 0, 1, 1, 0], [0, 0, 0, 0, 0, 2, 0])
    , ([2, 1, 1, 0, 1, 0, 2], [2, 0, 0, 0, 0, 1, 0])
    , ([2, 0, 0, 2, 0, 0, 2], [0, 0, 0, 1, 0, 0, 0])
    , ([2, 1, 1, 0, 1, 2, 2], [2, 0, 0, 0, 0, 0, 0])
    , ([2, 0, 0, 2, 0, 2, 0], [1, 0, 0, 0, 0, 1, 0])
    , ([2, 0, 2, 2, 0, 2, 0], [0, 0, 0, 0, 0, 0, 2])
    , ([2, 0, 0, 2, 0, 2, 2], [2, 0, 0, 0, 0, 0, 0])
    , ([2, 2, 2, 0, 2, 0, 2], [0, 0, 0, 0, 0, 1, 0])
    , ([2, 2, 2, 0, 2, 2, 2], [1, 0, 0, 0, 0, 0, 0])
    , ([2, 2, 2, 2, 2, 2, 2], [0, 0, 0, 0, 0, 0, 0])
    ]

  val E7_nilpotent_orbit_dual_table : (diagram * diagram) list =
    sortByKey E7_nilpotent_orbit_dual_table_unsorted

  fun E7_nilpotent_orbit_dual_map (d: diagram) : diagram =
    lookupSorted (E7_nilpotent_orbit_dual_table, d, "dual_map")

  val E7_Springer_table_unsorted : (diagram * int) list =
    [ ([0, 0, 0, 0, 0, 0, 0], 1)
    , ([1, 0, 0, 0, 0, 0, 0], 2)
    , ([0, 0, 0, 0, 0, 1, 0], 10)
    , ([0, 0, 0, 0, 0, 0, 2], 6)
    , ([0, 0, 1, 0, 0, 0, 0], 12)
    , ([2, 0, 0, 0, 0, 0, 0], 16)
    , ([0, 1, 0, 0, 0, 0, 1], 5)
    , ([1, 0, 0, 0, 0, 1, 0], 28)
    , ([0, 0, 0, 1, 0, 0, 0], 35)
    , ([2, 0, 0, 0, 0, 1, 0], 39)
    , ([0, 0, 0, 0, 0, 2, 0], 31)
    , ([0, 2, 0, 0, 0, 0, 0], 27)
    , ([2, 0, 0, 0, 0, 0, 2], 33)
    , ([0, 0, 1, 0, 0, 1, 0], 18)
    , ([1, 0, 0, 1, 0, 0, 0], 44)
    , ([0, 0, 2, 0, 0, 0, 0], 48)
    , ([1, 0, 0, 0, 1, 0, 1], 43)
    , ([2, 0, 2, 0, 0, 0, 0], 26)
    , ([0, 1, 1, 0, 0, 0, 1], 55)
    , ([0, 0, 0, 1, 0, 1, 0], 53)
    , ([2, 0, 0, 0, 0, 2, 0], 56)
    , ([0, 0, 0, 0, 2, 0, 0], 41)
    , ([2, 0, 0, 0, 0, 2, 2], 25)
    , ([2, 1, 1, 0, 0, 0, 1], 21)
    , ([1, 0, 0, 1, 0, 1, 0], 58)
    , ([2, 0, 0, 1, 0, 1, 0], 57)
    , ([0, 0, 0, 2, 0, 0, 0], 38)
    , ([1, 0, 0, 1, 0, 2, 0], 42)
    , ([1, 0, 0, 1, 0, 1, 2], 19)
    , ([2, 0, 0, 0, 2, 0, 0], 52)
    , ([0, 1, 1, 0, 1, 0, 2], 47)
    , ([0, 0, 2, 0, 0, 2, 0], 54)
    , ([2, 0, 2, 0, 0, 2, 0], 32)
    , ([0, 0, 0, 2, 0, 0, 2], 49)
    , ([0, 0, 0, 2, 0, 2, 0], 22)
    , ([2, 1, 1, 0, 1, 1, 0], 30)
    , ([2, 1, 1, 0, 1, 0, 2], 40)
    , ([2, 0, 0, 2, 0, 0, 2], 34)
    , ([2, 1, 1, 0, 1, 2, 2], 15)
    , ([2, 0, 0, 2, 0, 2, 0], 29)
    , ([2, 0, 2, 2, 0, 2, 0], 9)
    , ([2, 0, 0, 2, 0, 2, 2], 17)
    , ([2, 2, 2, 0, 2, 0, 2], 11)
    , ([2, 2, 2, 0, 2, 2, 2], 3)
    , ([2, 2, 2, 2, 2, 2, 2], 0)
    ]

  val E7_Springer_table : (diagram * int) list = sortByKey E7_Springer_table_unsorted

  fun E7_Springer_map (d: diagram) : int =
    lookupSorted (E7_Springer_table, d, "Springer_map")
end
