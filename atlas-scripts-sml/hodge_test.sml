use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/hodge_test.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/hodge_test.at`.
  - The `.at` script is a test/driver for the Hodge/Dirac computation pipeline.

  Status
  - Not yet ported: the Hodge pipeline scripts are not yet implemented in SML.
*)

structure Hodge_test = struct
  fun TODO (_: string) : 'a =
    raise Fail "Hodge_test: not yet ported"
end

