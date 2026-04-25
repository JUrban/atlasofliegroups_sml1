use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/conjugate.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/conjugate.at`.
  - The `.at` script is a substantial Weyl-group transporter/normalizer toolkit
    for (pseudo-)Levi subsystems, including enumerators for normalizers and
    witnesses of conjugacy.

  Status
  - Not yet ported: the script relies on a large collection of `.at` libraries
    (`Levi_subgroups`, `diagram`, `W_orbit`, `simple_factors`, etc.) and
    specialized Weyl-element representations (`WeylElt`) beyond what is
    currently available at the same abstraction level in SML.
*)

structure Conjugate = struct
  fun TODO (_: string) : 'a =
    raise Fail "Conjugate: not yet ported"
end

