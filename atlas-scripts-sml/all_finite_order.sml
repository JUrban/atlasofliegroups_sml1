use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/all_finite_order.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/all_finite_order.at`.
  - The `.at` script enumerates elements of finite order in certain lattices /
    tori and is used by some twisting/conjugacy computations.

  Status
  - Not yet ported: depends on cyclotomic-field / torus-element infrastructure
    and on interpreter-side helpers not currently present in the SML port.
*)

structure All_finite_order = struct
  fun TODO (_: string) : 'a =
    raise Fail "All_finite_order: not yet ported"
end

