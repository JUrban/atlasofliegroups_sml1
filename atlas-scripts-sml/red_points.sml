use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/red_points.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/red_points.at`.
  - The `.at` script is part of the reduction/verification driver tooling for
    large point sets (used in some FPP-related runs).

  Status
  - Not yet ported: this is a driver script layered on top of large `.at`
    pipelines (FPP/Dirac) that are not fully ported to SML.
*)

structure Red_points = struct
  fun TODO (_: string) : 'a =
    raise Fail "Red_points: not yet ported"
end

