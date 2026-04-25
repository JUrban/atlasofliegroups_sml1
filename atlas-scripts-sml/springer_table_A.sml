use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/springer_table_A.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/springer_table_A.at`.
  - The `.at` script implements Springer correspondence in type A, connecting
    nilpotent orbits (partitions) to Weyl-group representations.

  Status
  - Not yet ported: depends on nilpotent orbit types and the Springer-table
    infrastructure (`springer_tables.at`) which are not currently available in
    the SML port.
*)

structure Springer_table_A = struct
  fun TODO (_: string) : 'a =
    raise Fail "Springer_table_A: not yet ported"
end

