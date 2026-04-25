(*
  File: atlas-scripts-sml/springer_tables.sml

  Purpose
  - Diagram-level core API inspired by `atlas-scripts/springer_tables.at`.

  Background
  - In the Atlas `.at` environment, a `SpringerTable` packages:
      - a list of nilpotent orbits,
      - a duality map between orbits for `rd` and its dual,
      - a Springer correspondence map `orbit -> character index`,
      - plus convenience operations (special closure, inverse Springer, etc).

  What we can do in SML today
  - The current SML port does not yet implement the full nilpotent orbit layer
    (`ComplexNilpotent` values, `orbits(rd)`, `complex_nilpotent_from_diagram`,
    etc.).
  - However, several per-type scripts *do* provide explicit tables on
    *diagrams* (integer vectors) for:
      - duality: `diagram -> diagram`
      - Springer map: `diagram -> int`
  - This module therefore defines a small, useful “diagram Springer table”
    record type and a few generic derived operations.

  Scope
  - Everything here is pure and does not depend on the Atlas interpreter.
*)

structure Springer_tables = struct
  type diagram = int list

  type diagram_table =
    { dual_map: diagram -> diagram
    , dual_map_i: diagram -> diagram
    , springer: diagram -> int
    }

  fun make_involutive (dual_map: diagram -> diagram, springer: diagram -> int) : diagram_table =
    {dual_map = dual_map, dual_map_i = dual_map, springer = springer}

  fun special_closure (st: diagram_table, d: diagram) : diagram =
    (#dual_map_i st) ((#dual_map st) d)

  fun is_special (st: diagram_table, d: diagram) : bool =
    special_closure (st, d) = d

  fun springer_character_index (st: diagram_table, d: diagram) : int =
    (#springer st) d
end
