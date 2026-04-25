use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/extended_cross.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/extended_cross.at`.
  - The `.at` script implements cross actions for extended parameters.

  Status
  - Not yet ported: extended parameters and extended-group actions are only
    partially represented in the current SML port and not in this `.at`-style
    form.
*)

structure Extended_cross = struct
  fun TODO (_: string) : 'a =
    raise Fail "Extended_cross: not yet ported"
end

