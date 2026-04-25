use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/hodge_normalize.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/hodge_normalize.at`.
  - The `.at` script normalizes parameters in the “Hodge” pipeline using
    complex/imaginary reflection formulas in the `hodgeParamPol` encoding.

  Status
  - Not yet ported: depends on Hodge parameter polynomial types, `is_normal`,
    and the KL/character formula layer.
*)

structure Hodge_normalize = struct
  fun TODO (_: string) : 'a =
    raise Fail "Hodge_normalize: not yet ported"
end

