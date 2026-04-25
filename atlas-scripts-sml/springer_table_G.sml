use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/springer_table_G.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/springer_table_G.at`.
  - The `.at` script provides Springer correspondence data for type G2.

  Status
  - Not yet ported: requires Springer-table and nilpotent-orbit infrastructure
    not yet present in the SML port.
*)

structure Springer_table_G = struct
  fun TODO (_: string) : 'a =
    raise Fail "Springer_table_G: not yet ported"
end

