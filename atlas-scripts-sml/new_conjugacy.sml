use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/new_conjugacy.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/new_conjugacy.at`.
  - The `.at` script provides additional Weyl-group conjugacy computations.

  Status
  - Not yet ported: depends on full class-table machinery not currently present
    in the SML port.
*)

structure New_conjugacy = struct
  fun TODO (_: string) : 'a =
    raise Fail "New_conjugacy: not yet ported"
end

