use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/dirac_index.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/dirac_index.at`.
  - The `.at` script computes Dirac index information for representations.

  Status
  - Not yet ported: depends on the full Dirac/cohomology stack and on
    K-type-formula computations that are not currently implemented end-to-end
    in SML.
*)

structure Dirac_index = struct
  fun TODO (_: string) : 'a =
    raise Fail "Dirac_index: not yet ported"
end

