use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/stable.sml

  Purpose
  - Translation scaffold for `atlas-scripts/stable.at`.
  - See the `.at` file for intended stable (conjugacy/endoscopy) routines.

  Status
  - Not yet ported: this file currently defines only a placeholder structure.
*)

structure Stable = struct
  fun TODO (_: string) : 'a =
    raise Fail "Stable: not yet ported"
end

