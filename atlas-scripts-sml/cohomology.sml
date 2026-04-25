use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/cohomology.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/cohomology.at`.
  - The `.at` script contains cohomology/cohomological-induction related
    computations.

  Status
  - Not yet ported: cohomological induction (and its supporting data structures
    and algorithms) are not currently implemented in the SML port.
*)

structure Cohomology = struct
  fun TODO (_: string) : 'a =
    raise Fail "Cohomology: not yet ported"
end

