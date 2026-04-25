use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/LKT_form.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/LKT_form.at`.
  - The `.at` script provides utilities for working with lowest-K-type (LKT)
    formulas and related hermitian/Dirac form computations.

  Status
  - Not yet ported: depends on K-type formula infrastructure and (in the `.at`
    environment) derived operations on `KTypePol` that are only partially
    available in the SML port.
*)

structure LKT_form = struct
  type group = AtlasFFI.group
  type param = AtlasFFI.param

  fun TODO (_: string) : 'a =
    raise Fail "LKT_form: not yet ported"
end

