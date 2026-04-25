use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/cells.sml

  Purpose
  - Translation scaffold for `atlas-scripts/cells.at`.
  - The `.at` script concerns (Kazhdan–Lusztig / Lusztig) cell computations;
    see the original for details.

  Status
  - Not yet ported: this file currently defines only a placeholder structure.
*)

structure Cells = struct
  fun TODO (_: string) : 'a =
    raise Fail "Cells: not yet ported"
end

