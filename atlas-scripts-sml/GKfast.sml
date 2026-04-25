use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/GKfast.sml

  Purpose
  - Placeholder SML translation of `atlas-scripts/GKfast.at`.
  - The `.at` script defines an alternative, faster `GKfast(p)` computation:
      GKfast(p) = #posroots(p.root_datum) - degree(special_character(cell(p)))
    where `cell(p)` is the Weyl cell of `Finalize(p)` and the degree is read
    from a character table attached to a Levi root datum.

  Status
  - Not yet implemented in SML because it depends on substantial interpreter-
    level infrastructure that is not yet ported:
      - `cells.at` (W-cells, `W_cell_of`, Levi root data for cells)
      - character-table accessors (`special_character_inefficient`, degrees)
  - Once `cells.at` is ported (or equivalent C++/FFI shims are exposed), this
    module should be upgraded to compute the quantity as in the `.at` script.
*)

structure GKfast = struct
  type param = AtlasFFI.param

  fun GKfast (_: param) : int =
    raise Fail "GKfast.GKfast: not yet ported (requires cells/character-table infrastructure)"
end

