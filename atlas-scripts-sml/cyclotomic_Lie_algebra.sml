use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/cyclotomic_Lie_algebra.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/cyclotomic_Lie_algebra.at`.
  - The `.at` script defines Lie algebra computations over cyclotomic fields.

  Status
  - Not yet ported: cyclotomic field elements/matrices are not implemented in SML.
*)

structure Cyclotomic_Lie_algebra = struct
  fun TODO (_: string) : 'a =
    raise Fail "Cyclotomic_Lie_algebra: not yet ported"
end

