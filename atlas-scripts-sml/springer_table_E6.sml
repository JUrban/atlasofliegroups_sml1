use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/springer_table_E6.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/springer_table_E6.at`.
  - The `.at` script provides Springer correspondence data for type E6.

  Status
  - Not yet ported: requires Springer-table and nilpotent-orbit infrastructure
    that is not currently present in the SML port.
*)

structure Springer_table_E6 = struct
  fun TODO (_: string) : 'a =
    raise Fail "Springer_table_E6: not yet ported"
end

