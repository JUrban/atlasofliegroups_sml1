use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/goodroots.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/goodroots.at`.
  - The `.at` script computes “good roots” sets used in various algorithms
    (often related to induction/Dirac/unitarity pipelines).

  Status
  - Not yet ported: depends on several interpreter-level root-status utilities
    and higher-level scripts.
*)

structure Goodroots = struct
  fun TODO (_: string) : 'a =
    raise Fail "Goodroots: not yet ported"
end

