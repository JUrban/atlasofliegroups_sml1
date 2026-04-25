use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/test_ind_unip.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/test_ind_unip.at`.
  - The `.at` script is a test/driver for induced unipotent computations.

  Status
  - Not yet ported: depends on unipotent packet and induction infrastructure.
*)

structure Test_ind_unip = struct
  fun TODO (_: string) : 'a =
    raise Fail "Test_ind_unip: not yet ported"
end

