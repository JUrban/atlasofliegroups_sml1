use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/good_W_representatives.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/good_W_representatives.at`.
  - The `.at` script computes “good” Weyl group representatives using
    cyclotomic-field computations and twisted/folded root datum logic.

  Status
  - Not yet ported: cyclotomic-field infrastructure is not currently present
    in the SML port, and the full representative-selection logic has not been
    reconstructed.
*)

structure Good_W_representatives = struct
  fun TODO (_: string) : 'a =
    raise Fail "Good_W_representatives: not yet ported"
end

