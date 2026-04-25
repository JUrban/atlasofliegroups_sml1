use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/W_reps.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/W_reps.at`.
  - The `.at` script computes Weyl-group representations and related tables.

  Status
  - Not yet ported: depends on the full Weyl class-table/character-table stack
    (including symmetric powers and generic degrees) not yet present in SML.
*)

structure W_reps = struct
  fun TODO (_: string) : 'a =
    raise Fail "W_reps: not yet ported"
end

