use "atlas-scripts-sml/basic.sml";
use "atlas-scripts-sml/sort.sml";

(*
  File: atlas-scripts-sml/springer_table_E8.sml

  Purpose
  - Partial SML translation of `atlas-scripts/springer_table_E8.at`.
  - The `.at` script defines:
      - a duality map on nilpotent orbit diagrams (as integer vectors), and
      - a “Springer map” from orbit diagrams to character-table indices.

  What is ported here
  - The pure diagram-level lookup tables:
      - `E8_nilpotent_orbit_dual_map : diagram -> diagram`
      - `E8_Springer_map : diagram -> int`
  - These do not depend on the Atlas interpreter’s `ComplexNilpotent` or
    `SpringerTable` types; they operate only on diagrams represented as `int list`.

  Porting approach
  - We port `E8_Springer_table_data` and `springer_by_name_table_E8` as SML data,
    then reconstruct the same derived lookup tables as the `.at` code:
      - `name_to_diagram_E8 : string -> diagram`
      - `diagram -> dualDiagram` table
      - `diagram -> springerIndex` table
*)

structure Springer_table_E8 = struct
  type diagram = int list

  fun leqDiagram (a: diagram, b: diagram) : bool = Sort.rlex_leq (a, b)

  fun sortByDiagram (xs: (diagram * 'a) list) : (diagram * 'a) list =
    Basic.sort (fn ((a, _), (b, _)) => leqDiagram (a, b)) xs

  fun lookupSortedDiagram (table: (diagram * 'a) list, d: diagram, what: string) : 'a =
    let
      val n = length table
      fun keyAt i = #1 (List.nth (table, i))
      fun pred i = leqDiagram (d, keyAt i)
      val k = Basic.binary_search_first (pred, 0, n)
      val () = if k < n then () else raise Fail ("Springer_table_E8: " ^ what ^ ": unknown diagram")
      val (src, img) = List.nth (table, k)
      val () = if src = d then () else raise Fail ("Springer_table_E8: " ^ what ^ ": unknown diagram")
    in
      img
    end

  fun sortByName (xs: (string * 'a) list) : (string * 'a) list =
    Basic.sort (fn ((a, _), (b, _)) => a <= b) xs

  fun lookupSortedName (table: (string * 'a) list, key: string, what: string) : 'a =
    let
      val n = length table
      fun keyAt i = #1 (List.nth (table, i))
      fun pred i = key <= keyAt i
      val k = Basic.binary_search_first (pred, 0, n)
      val () = if k < n then () else raise Fail ("Springer_table_E8: " ^ what ^ ": unknown name")
      val (found, img) = List.nth (table, k)
      val () = if found = key then () else raise Fail ("Springer_table_E8: " ^ what ^ ": unknown name")
    in
      img
    end

  type entry =
    { name: string
    , diagram: diagram
    , dim: int
    , special: bool
    , dual: string
    , aa: string
    }

  val E8_Springer_table_data : entry list =
    [ {name = "0", diagram = [0, 0, 0, 0, 0, 0, 0, 0], dim = 0, special = true, dual = "E8", aa = "1"}
    , {name = "A1", diagram = [0, 0, 0, 0, 0, 0, 0, 1], dim = 58, special = true, dual = "E8(a1)", aa = "1"}
    , {name = "2A1", diagram = [1, 0, 0, 0, 0, 0, 0, 0], dim = 92, special = true, dual = "E8(a2)", aa = "1"}
    , {name = "3A1", diagram = [0, 0, 0, 0, 0, 0, 1, 0], dim = 112, special = false, dual = "E8(a3)", aa = "1"}
    , {name = "A2", diagram = [0, 0, 0, 0, 0, 0, 0, 2], dim = 114, special = true, dual = "E8(a3)", aa = "S2"}
    , {name = "4A1", diagram = [0, 1, 0, 0, 0, 0, 0, 0], dim = 128, special = false, dual = "E8(a4)", aa = "1"}
    , {name = "A2+A1", diagram = [1, 0, 0, 0, 0, 0, 0, 1], dim = 136, special = true, dual = "E8(a4)", aa = "S2"}
    , {name = "A2+2A1", diagram = [0, 0, 0, 0, 0, 1, 0, 0], dim = 146, special = true, dual = "E8(b4)", aa = "1"}
    , {name = "A3", diagram = [1, 0, 0, 0, 0, 0, 0, 2], dim = 148, special = true, dual = "E7(a1)", aa = "1"}
    , {name = "A2+3A1", diagram = [0, 0, 1, 0, 0, 0, 0, 0], dim = 154, special = false, dual = "E8(a5)", aa = "1"}
    , {name = "2A2", diagram = [2, 0, 0, 0, 0, 0, 0, 0], dim = 156, special = true, dual = "E8(a5)", aa = "S2"}
    , {name = "2A2+A1", diagram = [1, 0, 0, 0, 0, 0, 1, 0], dim = 162, special = false, dual = "E8(b5)", aa = "1"}
    , {name = "A3+A1", diagram = [0, 0, 0, 0, 0, 1, 0, 1], dim = 164, special = false, dual = "E8(b5)", aa = "1"}
    , {name = "D4(a1)", diagram = [0, 0, 0, 0, 0, 0, 2, 0], dim = 166, special = true, dual = "E8(b5)", aa = "S3"}
    , {name = "D4", diagram = [0, 0, 0, 0, 0, 0, 2, 2], dim = 168, special = true, dual = "E6", aa = "1"}
    , {name = "2A2+2A1", diagram = [0, 0, 0, 0, 1, 0, 0, 0], dim = 168, special = false, dual = "E8(a6)", aa = "1"}
    , {name = "A3+2A1", diagram = [0, 0, 1, 0, 0, 0, 0, 1], dim = 172, special = false, dual = "E8(a6)", aa = "1"}
    , {name = "D4(a1)+A1", diagram = [0, 1, 0, 0, 0, 0, 1, 0], dim = 176, special = true, dual = "E8(a6)", aa = "S3"}
    , {name = "A3+A2", diagram = [1, 0, 0, 0, 0, 1, 0, 0], dim = 178, special = true, dual = "D7(a1)", aa = "S2:1"}
    , {name = "A4", diagram = [2, 0, 0, 0, 0, 0, 0, 2], dim = 180, special = true, dual = "E7(a3)", aa = "S2"}
    , {name = "A3+A2+A1", diagram = [0, 0, 0, 1, 0, 0, 0, 0], dim = 182, special = false, dual = "E8(b6)", aa = "1"}
    , {name = "D4+A1", diagram = [0, 1, 0, 0, 0, 0, 1, 2], dim = 184, special = false, dual = "E6(a1)", aa = "1"}
    , {name = "D4(a1)+A2", diagram = [0, 2, 0, 0, 0, 0, 0, 0], dim = 184, special = true, dual = "E8(b6)", aa = "S2"}
    , {name = "A4+A1", diagram = [1, 0, 0, 0, 0, 1, 0, 1], dim = 188, special = true, dual = "E6(a1)+A1", aa = "S2"}
    , {name = "2A3", diagram = [1, 0, 0, 0, 1, 0, 0, 0], dim = 188, special = false, dual = "D7(a2)", aa = "1"}
    , {name = "D5(a1)", diagram = [1, 0, 0, 0, 0, 1, 0, 2], dim = 190, special = true, dual = "E6(a1)", aa = "S2"}
    , {name = "A4+2A1", diagram = [0, 0, 0, 1, 0, 0, 0, 1], dim = 192, special = true, dual = "D7(a2)", aa = "S2"}
    , {name = "A4+A2", diagram = [0, 0, 0, 0, 0, 2, 0, 0], dim = 194, special = true, dual = "D5+A2", aa = "1"}
    , {name = "A5", diagram = [2, 0, 0, 0, 0, 1, 0, 1], dim = 196, special = false, dual = "D6(a1)", aa = "1"}
    , {name = "D5(a1)+A1", diagram = [0, 0, 0, 1, 0, 0, 0, 2], dim = 196, special = true, dual = "E7(a4)", aa = "1"}
    , {name = "A4+A2+A1", diagram = [0, 0, 1, 0, 0, 1, 0, 0], dim = 196, special = true, dual = "A6+A1", aa = "1"}
    , {name = "D4+A2", diagram = [0, 2, 0, 0, 0, 0, 0, 2], dim = 198, special = true, dual = "A6", aa = "S2:1"}
    , {name = "E6(a3)", diagram = [2, 0, 0, 0, 0, 0, 2, 0], dim = 198, special = true, dual = "D6(a1)", aa = "S2"}
    , {name = "D5", diagram = [2, 0, 0, 0, 0, 0, 2, 2], dim = 200, special = true, dual = "D5", aa = "1"}
    , {name = "A4+A3", diagram = [0, 0, 0, 1, 0, 0, 1, 0], dim = 200, special = false, dual = "E8(a7)", aa = "1"}
    , {name = "A5+A1", diagram = [1, 0, 0, 1, 0, 0, 0, 1], dim = 202, special = false, dual = "E8(a7)", aa = "1"}
    , {name = "D5(a1)+A2", diagram = [0, 0, 1, 0, 0, 1, 0, 1], dim = 202, special = false, dual = "E8(a7)", aa = "1"}
    , {name = "D6(a2)", diagram = [0, 1, 1, 0, 0, 0, 1, 0], dim = 204, special = false, dual = "E8(a7)", aa = "S2"}
    , {name = "E6(a3)+A1", diagram = [1, 0, 0, 0, 1, 0, 1, 0], dim = 204, special = false, dual = "E8(a7)", aa = "S2"}
    , {name = "E7(a5)", diagram = [0, 0, 0, 1, 0, 1, 0, 0], dim = 206, special = false, dual = "E8(a7)", aa = "S3"}
    , {name = "D5+A1", diagram = [1, 0, 0, 0, 1, 0, 1, 2], dim = 208, special = false, dual = "E6(a3)", aa = "1"}
    , {name = "E8(a7)", diagram = [0, 0, 0, 0, 2, 0, 0, 0], dim = 208, special = true, dual = "E8(a7)", aa = "S5"}
    , {name = "A6", diagram = [2, 0, 0, 0, 0, 2, 0, 0], dim = 210, special = true, dual = "D4+A2", aa = "1"}
    , {name = "D6(a1)", diagram = [0, 1, 1, 0, 0, 0, 1, 2], dim = 210, special = true, dual = "E6(a3)", aa = "S2"}
    , {name = "A6+A1", diagram = [1, 0, 0, 1, 0, 1, 0, 0], dim = 212, special = true, dual = "A4+A2+A1", aa = "1"}
    , {name = "E7(a4)", diagram = [0, 0, 0, 1, 0, 1, 0, 2], dim = 212, special = true, dual = "D5(a1)+A1", aa = "S2:1"}
    , {name = "E6(a1)", diagram = [2, 0, 0, 0, 0, 2, 0, 2], dim = 214, special = true, dual = "D5(a1)", aa = "S2"}
    , {name = "D5+A2", diagram = [0, 0, 0, 0, 2, 0, 0, 2], dim = 214, special = true, dual = "A4+A2", aa = "S2:1"}
    , {name = "D6", diagram = [2, 1, 1, 0, 0, 0, 1, 2], dim = 216, special = false, dual = "A4", aa = "1"}
    , {name = "E6", diagram = [2, 0, 0, 0, 0, 2, 2, 2], dim = 216, special = true, dual = "D4", aa = "1"}
    , {name = "D7(a2)", diagram = [1, 0, 0, 1, 0, 1, 0, 1], dim = 216, special = true, dual = "A4+2A1", aa = "S2"}
    , {name = "A7", diagram = [1, 0, 0, 1, 0, 1, 1, 0], dim = 218, special = false, dual = "D4(a1)+A2", aa = "1"}
    , {name = "E6(a1)+A1", diagram = [1, 0, 0, 1, 0, 1, 0, 2], dim = 218, special = true, dual = "A4+A1", aa = "S2"}
    , {name = "E7(a3)", diagram = [2, 0, 0, 1, 0, 1, 0, 2], dim = 220, special = true, dual = "A4", aa = "S2"}
    , {name = "E8(b6)", diagram = [0, 0, 0, 2, 0, 0, 0, 2], dim = 220, special = true, dual = "D4(a1)+A2", aa = "S3:S2"}
    , {name = "D7(a1)", diagram = [2, 0, 0, 0, 2, 0, 0, 2], dim = 222, special = true, dual = "A3+A2", aa = "S2:1"}
    , {name = "E6+A1", diagram = [1, 0, 0, 1, 0, 1, 2, 2], dim = 222, special = false, dual = "D4(a1)", aa = "1"}
    , {name = "E7(a2)", diagram = [0, 1, 1, 0, 1, 0, 2, 2], dim = 224, special = false, dual = "D4(a1)", aa = "1"}
    , {name = "E8(a6)", diagram = [0, 0, 0, 2, 0, 0, 2, 0], dim = 224, special = true, dual = "D4(a1)+A1", aa = "S3"}
    , {name = "D7", diagram = [2, 1, 1, 0, 1, 1, 0, 1], dim = 226, special = false, dual = "2A2", aa = "1"}
    , {name = "E8(b5)", diagram = [0, 0, 0, 2, 0, 0, 2, 2], dim = 226, special = true, dual = "D4(a1)", aa = "S3"}
    , {name = "E7(a1)", diagram = [2, 1, 1, 0, 1, 0, 2, 2], dim = 228, special = true, dual = "A3", aa = "1"}
    , {name = "E8(a5)", diagram = [2, 0, 0, 2, 0, 0, 2, 0], dim = 228, special = true, dual = "2A2", aa = "S2"}
    , {name = "E8(b4)", diagram = [2, 0, 0, 2, 0, 0, 2, 2], dim = 230, special = true, dual = "A2+2A1", aa = "S2:1"}
    , {name = "E7", diagram = [2, 1, 1, 0, 1, 2, 2, 2], dim = 232, special = false, dual = "A2", aa = "1"}
    , {name = "E8(a4)", diagram = [2, 0, 0, 2, 0, 2, 0, 2], dim = 232, special = true, dual = "A2+A1", aa = "S2"}
    , {name = "E8(a3)", diagram = [2, 0, 0, 2, 0, 2, 2, 2], dim = 234, special = true, dual = "A2", aa = "S2"}
    , {name = "E8(a2)", diagram = [2, 2, 2, 0, 2, 0, 2, 2], dim = 236, special = true, dual = "2A1", aa = "1"}
    , {name = "E8(a1)", diagram = [2, 2, 2, 0, 2, 2, 2, 2], dim = 238, special = true, dual = "A1", aa = "1"}
    , {name = "E8", diagram = [2, 2, 2, 2, 2, 2, 2, 2], dim = 240, special = true, dual = "0", aa = "1"}
    ]

  val name_to_diagram_table : (string * diagram) list =
    sortByName (List.map (fn e => (#name e, #diagram e)) E8_Springer_table_data)

  fun name_to_diagram_E8 (name: string) : diagram =
    lookupSortedName (name_to_diagram_table, name, "name_to_diagram")

  val E8_nilpotent_orbit_dual_table : (diagram * diagram) list =
    sortByDiagram
      (List.map
         (fn e => (#diagram e, name_to_diagram_E8 (#dual e)))
         E8_Springer_table_data)

  fun E8_nilpotent_orbit_dual_map (d: diagram) : diagram =
    lookupSortedDiagram (E8_nilpotent_orbit_dual_table, d, "dual_map")

  val springer_by_name_table_E8 : (string * int) list =
    [ ("0", 1)
    , ("A1", 68)
    , ("2A1", 5)
    , ("3A1", 10)
    , ("A2", 72)
    , ("4A1", 8)
    , ("A2+A1", 15)
    , ("A2+2A1", 81)
    , ("A3", 24)
    , ("A2+3A1", 77)
    , ("2A2", 29)
    , ("2A2+A1", 79)
    , ("A3+A1", 40)
    , ("D4(a1)", 93)
    , ("D4", 22)
    , ("2A2+2A1", 13)
    , ("A3+2A1", 37)
    , ("D4(a1)+A1", 43)
    , ("A3+A2", 100)
    , ("A4", 54)
    , ("A3+A2+A1", 91)
    , ("D4+A1", 27)
    , ("D4(a1)+A2", 51)
    , ("A4+A1", 62)
    , ("2A3", 32)
    , ("D5(a1)", 97)
    , ("A4+2A1", 64)
    , ("A4+A2", 109)
    , ("A5", 60)
    , ("D5(a1)+A1", 66)
    , ("A4+A2+A1", 57)
    , ("D4+A2", 107)
    , ("E6(a3)", 111)
    , ("D5", 38)
    , ("A4+A3", 16)
    , ("A5+A1", 87)
    , ("D5(a1)+A2", 82)
    , ("D6(a2)", 49)
    , ("E6(a3)+A1", 46)
    , ("E7(a5)", 103)
    , ("D5+A1", 59)
    , ("E8(a7)", 52)
    , ("A6", 106)
    , ("D6(a1)", 110)
    , ("A6+A1", 56)
    , ("E7(a4)", 65)
    , ("E6(a1)", 96)
    , ("D5+A2", 108)
    , ("D6", 34)
    , ("E6", 21)
    , ("D7(a2)", 63)
    , ("A7", 90)
    , ("E6(a1)+A1", 104)
    , ("E7(a3)", 53)
    , ("E8(b6)", 50)
    , ("D7(a1)", 99)
    , ("E6+A1", 78)
    , ("E7(a2)", 39)
    , ("E8(a6)", 42)
    , ("D7", 76)
    , ("E8(b5)", 92)
    , ("E7(a1)", 23)
    , ("E8(a5)", 28)
    , ("E8(b4)", 80)
    , ("E7", 9)
    , ("E8(a4)", 14)
    , ("E8(a3)", 71)
    , ("E8(a2)", 4)
    , ("E8(a1)", 67)
    , ("E8", 0)
    ]

  val E8_Springer_table : (diagram * int) list =
    sortByDiagram (List.map (fn (nm, idx) => (name_to_diagram_E8 nm, idx)) springer_by_name_table_E8)

  fun E8_Springer_map (d: diagram) : int =
    lookupSortedDiagram (E8_Springer_table, d, "Springer_map")
end
