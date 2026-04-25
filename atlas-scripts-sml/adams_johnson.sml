use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/adams_johnson.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/adams_johnson.at`.
  - The `.at` script implements computations around Adams–Johnson packets and
    related cohomological constructions.

  Status
  - Not yet ported: depends on cohomological induction, packet enumeration,
    and stable-sum tooling not yet present in the SML port.
*)

structure Adams_johnson = struct
  fun TODO (_: string) : 'a =
    raise Fail "Adams_johnson: not yet ported"
end

