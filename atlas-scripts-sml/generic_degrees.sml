use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/generic_degrees.sml

  Purpose
  - Placeholder SML translation of `atlas-scripts/generic_degrees.at`.
  - The `.at` script checks/recovers generic degrees of Weyl-group
    representations by decomposing cell characters into irreducibles and
    comparing to `ct.generic_degree(i)`.

  Status
  - Not yet implemented in SML because it depends on:
      - `cells.at` (W-cells and `cell_character`)
      - a rich `CharacterTable` API (decomposition, degree, generic degrees)
  - The SML codebase currently contains partial character-table utilities for
    some types, but not the full general API required by this script.
*)

structure GenericDegrees = struct
  type character_table = unit

  fun generic_degrees (_: character_table, _: unit list) : int list =
    raise Fail "GenericDegrees.generic_degrees: not yet ported (requires cells/CharacterTable APIs)"
end
