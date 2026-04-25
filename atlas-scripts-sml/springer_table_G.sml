use "atlas-scripts-sml/basic.sml";
use "atlas-scripts-sml/springer_tables.sml";

(*
  File: atlas-scripts-sml/springer_table_G.sml

  Purpose
  - Partial SML translation of `atlas-scripts/springer_table_G.at`.
  - The `.at` script’s `springer_table_G` definition uses a case split on
    the orbit diagram for the “Springer map”, and a small ad hoc dual mapping.

  What is ported here
  - Diagram-level maps (no `ComplexNilpotent`/`SpringerTable` types):
      - `G2_Springer_map : diagram -> int`
      - `G2_nilpotent_orbit_dual_map : diagram -> diagram`
*)

structure Springer_table_G = struct
  type diagram = int list

  fun G2_Springer_map (d: diagram) : int =
    case d of
      [0, 0] => 3
    | [0, 1] => 1
    | [0, 2] => 5
    | [1, 0] => 4
    | [2, 2] => 0
    | _ => raise Fail "Springer_table_G: unknown G2 diagram"

  (* Diagram-level duality extracted from the comment/logic in `springer_table_G.at`
     for the simply-connected `G2` numbering. *)
  fun G2_nilpotent_orbit_dual_map (d: diagram) : diagram =
    case d of
      [0, 0] => [2, 2]
    | [2, 2] => [0, 0]
    | [0, 1] => [0, 2]
    | [1, 0] => [0, 2]
    | [0, 2] => [0, 2]
    | _ => raise Fail "Springer_table_G: unknown G2 diagram"

  val diagram_table : Springer_tables.diagram_table =
    Springer_tables.make_involutive (G2_nilpotent_orbit_dual_map, G2_Springer_map)
end
