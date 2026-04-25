use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/finite_unipotents.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/finite_unipotents.at`.
  - The `.at` script enumerates finite-dimensional unipotent representations.

  Status
  - Not yet ported: relies on unipotent/Arthur packet infrastructure.
*)

structure Finite_unipotents = struct
  fun TODO (_: string) : 'a =
    raise Fail "Finite_unipotents: not yet ported"
end

