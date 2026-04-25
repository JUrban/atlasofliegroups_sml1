use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/hodge_tensor.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/hodge_tensor.at`.
  - The `.at` script defines tensor-product constructions within the Hodge
    parameter polynomial framework.

  Status
  - Not yet ported: requires `hodgeParamPol` types and operations.
*)

structure Hodge_tensor = struct
  fun TODO (_: string) : 'a =
    raise Fail "Hodge_tensor: not yet ported"
end

