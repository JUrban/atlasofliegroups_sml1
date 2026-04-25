use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/test_packet_induction.sml

  Purpose
  - Placeholder SML translation of `atlas-scripts/test_packet_induction.at`.
  - The `.at` script relies on `weak_packets.at` and parabolic induction to
    enumerate weak packets induced from Levi subgroups.

  Status
  - Not yet implemented in SML because `weak_packets.at` and the associated
    packet infrastructure are not ported.
*)

structure TestPacketInduction = struct
  type group = AtlasFFI.group

  fun weak_packets_parabolically_induced (_: group, _: int list) : AtlasFFI.param list =
    raise Fail "test_packet_induction: not yet ported (depends on weak_packets)"
end
