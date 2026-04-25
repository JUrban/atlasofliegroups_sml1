use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/smallQ.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/smallQ.at`.
  - The `.at` script provides specialized helpers for theta-stable parabolic
    data with a “no C-” normalization step.

  Status
  - Not yet ported: depends on `delta_on_simple`, `theta_stable_parabolic`,
    `theta_induce_standard`, and other higher-level `.at` constructs not yet
    available in SML.
*)

structure SmallQ = struct
  fun TODO (_: string) : 'a =
    raise Fail "SmallQ: not yet ported"
end

