use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/special_rep.sml

  Purpose
  - Translation scaffold for `atlas-scripts/special_rep.at`.
  - See the `.at` file for intended “special representation” computations.

  Status
  - Not yet ported: this file currently defines only a placeholder structure.
*)

structure Special_rep = struct
  fun TODO (_: string) : 'a =
    raise Fail "Special_rep: not yet ported"
end

