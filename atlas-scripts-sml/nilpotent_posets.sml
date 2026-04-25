use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/nilpotent_posets.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/nilpotent_posets.at`.
  - The `.at` script defines the poset of nilpotent orbit closures and related
    graph/closure utilities.

  Status
  - Not yet ported: nilpotent orbit types and closure data are not yet
    implemented in the SML port.
*)

structure Nilpotent_posets = struct
  fun TODO (_: string) : 'a =
    raise Fail "Nilpotent_posets: not yet ported"
end

