use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/steve.sml

  Purpose
  - Translation scaffold for `atlas-scripts/steve.at`.
  - See the `.at` file for intended functionality (script-specific utilities).

  Status
  - Not yet ported: this file currently defines only a placeholder structure.
*)

structure Steve = struct
  fun TODO (_: string) : 'a =
    raise Fail "Steve: not yet ported"
end

