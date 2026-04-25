use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/classical_W_classes_and_reps.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/classical_W_classes_and_reps.at`.
  - The `.at` script provides Weyl-group conjugacy classes and representative
    computations for classical types.

  Status
  - Not yet ported: while the SML port has some Weyl-group infrastructure
    (`WeylElt`, `Bruhat`, conjugacy-class partial order), it does not yet have
    the full class-table construction utilities for all classical types.
*)

structure Classical_W_classes_and_reps = struct
  fun TODO (_: string) : 'a =
    raise Fail "Classical_W_classes_and_reps: not yet ported"
end

