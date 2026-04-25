use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/springer_table_F.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/springer_table_F.at`.
  - The `.at` script provides Springer correspondence data for type F4.

  Status
  - Not yet ported: depends on Springer-table and nilpotent-orbit layers not
    currently present in the SML port.
*)

structure Springer_table_F = struct
  fun TODO (_: string) : 'a =
    raise Fail "Springer_table_F: not yet ported"
end

