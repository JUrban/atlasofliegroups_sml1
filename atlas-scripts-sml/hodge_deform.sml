use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/hodge_deform.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/hodge_deform.at`.
  - The `.at` script defines deformation operations in the Hodge parameter
    polynomial encoding.

  Status
  - Not yet ported: depends on `hodgeParamPol` and the Hodge normalization
    pipeline not currently implemented in SML.
*)

structure Hodge_deform = struct
  fun TODO (_: string) : 'a =
    raise Fail "Hodge_deform: not yet ported"
end

