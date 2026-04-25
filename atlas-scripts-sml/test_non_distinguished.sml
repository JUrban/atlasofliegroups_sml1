use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/test_non_distinguished.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/test_non_distinguished.at`.
  - The `.at` script tests certain non-distinguished nilpotent/orbit/packet
    scenarios.

  Status
  - Not yet ported: relies on nilpotent orbit and weak packet infrastructure.
*)

structure Test_non_distinguished = struct
  fun TODO (_: string) : 'a =
    raise Fail "Test_non_distinguished: not yet ported"
end

