use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/cyclotomic_Gaussian_elim.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/cyclotomic_Gaussian_elim.at`.
  - The `.at` script implements Gaussian elimination over cyclotomic fields.

  Status
  - Not yet ported: the SML port currently does not implement cyclotomic field
    elements/matrices (`cyclotomicMat.at`), so this file is a placeholder.
*)

structure Cyclotomic_Gaussian_elim = struct
  fun TODO (_: string) : 'a =
    raise Fail "Cyclotomic_Gaussian_elim: not yet ported"
end

