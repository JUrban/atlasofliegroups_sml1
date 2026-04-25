use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/cyclotomic.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/cyclotomic.at`.
  - The `.at` script defines cyclotomic field elements and operations used by
    character evaluations and “good representative” computations.

  Status
  - Not yet ported: cyclotomic field elements are not yet implemented in SML.
*)

structure Cyclotomic = struct
  fun TODO (_: string) : 'a =
    raise Fail "Cyclotomic: not yet ported"
end

