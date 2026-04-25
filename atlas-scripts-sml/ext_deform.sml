use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/ext_deform.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/ext_deform.at`.
  - The `.at` script extends deformation computations to extended parameters.

  Status
  - Not yet ported: extended-parameter deformation requires the extended group
    layer (`extended.at`, `extended_types.at`) and extended deformation
    primitives not currently exposed through the SML FFI.
*)

structure Ext_deform = struct
  fun TODO (_: string) : 'a =
    raise Fail "Ext_deform: not yet ported"
end

