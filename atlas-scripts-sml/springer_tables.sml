use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/springer_tables.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/springer_tables.at`.
  - The `.at` script defines the `SpringerTable` type and core operations
    (Springer map, dual map, orbit lists, etc.) used by per-type tables.

  Status
  - Not yet ported: Springer-table and nilpotent-orbit types are not yet
    implemented in the SML port.
*)

structure Springer_tables = struct
  fun TODO (_: string) : 'a =
    raise Fail "Springer_tables: not yet ported"
end

