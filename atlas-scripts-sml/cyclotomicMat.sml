use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/cyclotomicMat.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/cyclotomicMat.at`.
  - The `.at` script defines cyclotomic matrices/vectors, used by cyclotomic
    Gaussian elimination and Lie algebra computations.

  Status
  - Not yet ported: cyclotomic field elements are not yet implemented in SML.
*)

structure CyclotomicMat = struct
  fun TODO (_: string) : 'a =
    raise Fail "CyclotomicMat: not yet ported"
end

