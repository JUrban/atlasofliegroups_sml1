use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/alcove.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/alcove.at`.
  - The `.at` script implements “alcove” computations for deformation units and
    related wall-crossing logic.

  Status
  - Not yet ported: depends on `walls`, affine Weyl group algorithms, and
    deformation pipelines not currently implemented in the SML port.
*)

structure Alcove = struct
  fun TODO (_: string) : 'a =
    raise Fail "Alcove: not yet ported"
end

