use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/weak_packet_reports.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/weak_packet_reports.at`.
  - The `.at` script generates reports for weak packets and related stable-sum
    computations.

  Status
  - Not yet ported: depends on the weak-packets library and stable-sum tooling
    which are not currently available in the SML port.
*)

structure Weak_packet_reports = struct
  fun TODO (_: string) : 'a =
    raise Fail "Weak_packet_reports: not yet ported"
end

