use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/disconnected.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/disconnected.at`.
  - The `.at` script contains utilities for working with disconnected real
    groups and their representations.

  Status
  - Not yet ported: disconnected-group support is not yet exposed via the
    current SML-facing Atlas FFI.
*)

structure Disconnected = struct
  type group = AtlasFFI.group
  type param = AtlasFFI.param

  fun TODO (_: string) : 'a =
    raise Fail "Disconnected: not yet ported"
end

