use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/exceptional.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/exceptional.at`.
  - The `.at` script collects exceptional-type utilities and examples.

  Status
  - Not yet ported: exceptional-type workflows depend on data tables and
    packet/orbit computations not currently implemented in the SML port.
*)

structure Exceptional = struct
  fun TODO (_: string) : 'a =
    raise Fail "Exceptional: not yet ported"
end

