use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/nonintegral.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/nonintegral.at`.
  - The `.at` script provides machinery for dealing with non-integral
    infinitesimal characters and related normalization/crossing utilities.

  Status
  - Not yet ported: depends on the full `alcove`/walls and KL/coherent
    continuation infrastructure not currently implemented in SML.
*)

structure Nonintegral = struct
  fun TODO (_: string) : 'a =
    raise Fail "Nonintegral: not yet ported"
end

