use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/springer_tables_reductive.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/springer_tables_reductive.at`.
  - The `.at` script assembles Springer tables for a reductive root datum by
    decomposing into simple factors and combining the corresponding tables.

  Status
  - Not yet ported: requires a substantial Springer-table/nilpotent-orbit layer
    (`springer_tables.at` plus per-type tables) which is not currently present
    in the SML port.
*)

structure Springer_tables_reductive = struct
  fun TODO (_: string) : 'a =
    raise Fail "Springer_tables_reductive: not yet ported"
end

