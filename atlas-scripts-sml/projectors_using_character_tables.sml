use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/projectors_using_character_tables.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/projectors_using_character_tables.at`.
  - The `.at` script constructs certain projectors using Weyl character tables.

  Status
  - Not yet ported: requires a much richer character-table API (symmetric
    powers, generic degrees, projector computations) than the current SML
    `CharacterTables` port provides.
*)

structure Projectors_using_character_tables = struct
  fun TODO (_: string) : 'a =
    raise Fail "Projectors_using_character_tables: not yet ported"
end

