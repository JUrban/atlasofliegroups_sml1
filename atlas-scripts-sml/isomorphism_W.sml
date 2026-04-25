use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/isomorphism_W.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/isomorphism_W.at`.
  - The `.at` script contains Weyl-group isomorphism helpers (e.g. mapping
    between different root data / labeling conventions).

  Status
  - Not yet ported: the current SML port focuses on concrete Weyl actions
    within a fixed group handle. Cross-rootdatum “isomorphism” utilities have
    not yet been exposed in an `.at`-compatible way.
*)

structure Isomorphism_W = struct
  fun TODO (_: string) : 'a =
    raise Fail "Isomorphism_W: not yet ported"
end

