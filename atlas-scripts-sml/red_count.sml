use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/red_count.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/red_count.at`.
  - The `.at` script performs counting/reporting related to reductions in large
    verification runs.

  Status
  - Not yet ported: depends on the high-level FPP/Dirac workflows implemented
    in `.at` scripts.
*)

structure Red_count = struct
  fun TODO (_: string) : 'a =
    raise Fail "Red_count: not yet ported"
end

