use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/jantzen.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/jantzen.at`.
  - The `.at` script provides Jantzen filtration-related computations and
    related reducibility utilities.

  Status
  - Not yet ported: depends on KL/composition series and deeper representation
    theory infrastructure not currently implemented in the SML port.
*)

structure Jantzen = struct
  fun TODO (_: string) : 'a =
    raise Fail "Jantzen: not yet ported"
end

