use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/kl.sml

  Purpose
  - Translation scaffold for `atlas-scripts/kl.at`.
  - The `.at` script typically provides Kazhdan–Lusztig helper routines.

  Status
  - Not yet ported as a single monolithic module; see the existing partial SML
    ports such as `KL_polynomial_matrices.sml` and `dual.sml`.
*)

structure Kl = struct
  fun TODO (_: string) : 'a =
    raise Fail "Kl: not yet ported"
end

