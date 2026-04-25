use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/weakly_unipotent_LMB.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/weakly_unipotent_LMB.at`.
  - The `.at` script is one of several “weakly unipotent” heuristic variants.

  Status
  - Not yet ported: depends on KL character formulas and translation functors.
*)

structure Weakly_unipotent_LMB = struct
  fun TODO (_: string) : 'a =
    raise Fail "Weakly_unipotent_LMB: not yet ported"
end

