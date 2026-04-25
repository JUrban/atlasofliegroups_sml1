use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/hecke.sml

  Purpose
  - Translation scaffold for `atlas-scripts/hecke.at`.
  - The `.at` script defines or uses Hecke algebra operations and/or
    Kazhdan–Lusztig polynomials; see the original for details.

  Status
  - Not yet ported: this file currently defines only a placeholder structure.
*)

structure Hecke = struct
  fun TODO (_: string) : 'a =
    raise Fail "Hecke: not yet ported"
end

