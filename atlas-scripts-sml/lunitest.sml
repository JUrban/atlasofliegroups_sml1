use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/lunitest.sml

  Purpose
  - Placeholder SML translation of `atlas-scripts/lunitest.at`.
  - The `.at` script iterates over “semirigid” complex nilpotent data for a
    real form `G`, enumerates parameters with a fixed infinitesimal character,
    filters by `tau(p)` and `GK_dim(p)`, then counts which are unitary.

  Status
  - Not yet implemented in SML because it depends on unported components:
      - `rigid_unipotents.at`, `nilpotent_orbits.at`, `associated_variety_annihilator.at`
      - `GK_dim`, `tau(p)`, and semirigid-data tables
  - Once nilpotent-orbit and GK-dimension infrastructure is ported (or exposed
    via FFI), this script can be upgraded to a runnable SML analogue.
*)

structure LUniTest = struct
  type group = AtlasFFI.group

  fun testall (_: group) : unit =
    raise Fail "LUniTest.testall: not yet ported (depends on nilpotent/unipotent infrastructure)"
end

