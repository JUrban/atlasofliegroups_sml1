use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/verma.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/verma.at`.
  - The `.at` script computes invariants of irreducible highest-weight (Verma)
    modules via:
      - integral Weyl group cells,
      - Springer correspondence,
      - generic degrees / special representations.

  Status
  - Not yet ported: requires `cells.at`, `springer_tables*.at`,
    `character_tables.at` functionality (generic degrees, special reps), and
    several orbit/cell utilities that are still missing in SML.
*)

structure Verma = struct
  type rootdatum = AtlasFFI.rootdatum
  type param = AtlasFFI.param

  fun TODO (_: string) : 'a =
    raise Fail "Verma: not yet ported"
end

