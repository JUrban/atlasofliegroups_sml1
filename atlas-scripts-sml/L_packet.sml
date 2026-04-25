use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/L_packet.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/L_packet.at`.
  - The `.at` script defines utilities around L-packets and stable sums.

  Status
  - Not yet ported: packet enumeration and stable-sum tooling are not currently
    implemented in the SML port.
*)

structure L_packet = struct
  fun TODO (_: string) : 'a =
    raise Fail "L_packet: not yet ported"
end

