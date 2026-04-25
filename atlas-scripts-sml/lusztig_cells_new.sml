use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/lusztig_cells_new.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/lusztig_cells_new.at`.
  - The `.at` script computes Lusztig cells (newer variant).

  Status
  - Not yet ported: requires KL/cell combinatorics and related data that are
    not currently available in the SML port.
*)

structure Lusztig_cells_new = struct
  fun TODO (_: string) : 'a =
    raise Fail "Lusztig_cells_new: not yet ported"
end

