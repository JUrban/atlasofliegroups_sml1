use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/sub_cells.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/sub_cells.at`.
  - The `.at` script provides utilities for cell decompositions and sub-cell
    computations (e.g. left/right cells).

  Status
  - Not yet ported: cell infrastructure (`cells.at`, `kl.at`) is not currently
    implemented in the SML port.
*)

structure Sub_cells = struct
  fun TODO (_: string) : 'a =
    raise Fail "Sub_cells: not yet ported"
end

