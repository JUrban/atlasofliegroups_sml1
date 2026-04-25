use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/finite_dimensional_signature.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/finite_dimensional_signature.at`.
  - The `.at` script studies Frobenius–Schur indicators and Hermitian-form
    signatures for finite-dimensional representations, with a faster variant
    based on the Weyl character formula in the equal-rank case.

  Status
  - Not yet ported: the full functionality depends on:
      - Weyl character formula tracing (`weyl_character_formula.at`)
      - K-type dimension computations for branched signatures
      - `w0` (long element) action helpers
    which are not currently available in the SML port.
*)

structure Finite_dimensional_signature = struct
  fun TODO (_: string) : 'a =
    raise Fail "Finite_dimensional_signature: not yet ported"
end

