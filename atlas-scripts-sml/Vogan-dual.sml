use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/Vogan-dual.sml

  Purpose
  - Placeholder SML translation of `atlas-scripts/Vogan-dual.at`.
  - The `.at` script provides Vogan duality operations between a group and its
    dual, including dual KGB transport routines.

  Status
  - Not yet implemented in SML. Some duality-related primitives exist in the
    Poly/ML FFI (e.g. `atlas_group_dual_*` and `atlas_param_*` dual queries),
    but the script-level API surface has not been recreated.
*)

structure VoganDual = struct
  type group = AtlasFFI.group
  type param = AtlasFFI.param

  fun dual_KGB (_: param) : int =
    raise Fail "VoganDual.dual_KGB: not yet ported"
end

