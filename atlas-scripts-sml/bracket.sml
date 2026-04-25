use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/bracket.sml

  Purpose
  - Translation scaffold for `atlas-scripts/bracket.at`.
  - The `.at` script relates to Lie algebra bracket computations and/or
    structure-constant manipulations; see the original for details.

  Status
  - Not yet ported: this file currently defines only a placeholder structure.
*)

structure Bracket = struct
  fun TODO (_: string) : 'a =
    raise Fail "Bracket: not yet ported"
end

