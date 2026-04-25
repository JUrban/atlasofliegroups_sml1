use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/e8_magma.sml

  Purpose
  - Translation scaffold for `atlas-scripts/e8_magma.at`.
  - The `.at` script interfaces with or formats data for E8 computations; see
    the original for details.

  Status
  - Not yet ported: this file currently defines only a placeholder structure.
*)

structure E8_magma = struct
  fun TODO (_: string) : 'a =
    raise Fail "E8_magma: not yet ported"
end

