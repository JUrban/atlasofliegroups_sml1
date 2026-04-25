use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/real_component_groups.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/real_component_groups.at`.
  - The `.at` script is built around real/complex nilpotent orbits and computes
    various lists of KGB elements and real forms associated to pseudo-Levi data.

  Status
  - Not yet ported: this depends on the nilpotent-orbit stack (`nilpotent_orbits.at`)
    and many higher-level group/orbit operations not currently available in the
    SML port.
*)

structure Real_component_groups = struct
  fun TODO (_: string) : 'a =
    raise Fail "Real_component_groups: not yet ported"
end

