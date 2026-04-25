use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/extended_types.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/extended_types.at`.
  - The `.at` script defines extended-group data types and operations used in
    twisted endoscopy and extended KL computations.

  Status
  - Not yet ported: extended groups and their parameter types are only
    minimally represented in the current SML FFI layer (where present at all).
*)

structure Extended_types = struct
  fun TODO (_: string) : 'a =
    raise Fail "Extended_types: not yet ported"
end

