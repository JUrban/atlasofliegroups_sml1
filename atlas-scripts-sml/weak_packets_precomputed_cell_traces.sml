use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/weak_packets_precomputed_cell_traces.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/weak_packets_precomputed_cell_traces.at`.
  - The `.at` script uses precomputed Kazhdan–Lusztig cell traces to speed up
    weak-packet computations.

  Status
  - Not yet ported: weak-packet infrastructure and cell-trace precomputations
    are not currently available in the SML port.
*)

structure Weak_packets_precomputed_cell_traces = struct
  fun TODO (_: string) : 'a =
    raise Fail "Weak_packets_precomputed_cell_traces: not yet ported"
end

