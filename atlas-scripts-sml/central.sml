use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/central.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/central.at`.
  - The `.at` script defines a `CentralCharacter` type (for the real group
    center) and utilities for torsion/split/compact radical parts.

  Status
  - Not yet ported: it depends on the Atlas interpreter’s `InnerClass` /
    `RealForm` object model and on center computations not currently exposed
    in this form through the SML API.

  Related
  - For complex root data, see `atlas-scripts-sml/center.sml` (`structure Center`).
*)

structure Central = struct
  fun TODO (_: string) : 'a =
    raise Fail "Central: not yet ported"
end

