use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/nilpotent_induction.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/nilpotent_induction.at`.
  - The `.at` script studies (complex) nilpotent induction and related
    orbit/Levi combinatorics.

  Status
  - Not yet ported: the SML layer does not currently include the core
    `nilpotent_orbits.at` / Springer / orbit-poset infrastructure that this
    script depends on.
*)

structure NilpotentInduction = struct
  type group = AtlasFFI.group
  type rootdatum = AtlasFFI.rootdatum

  fun TODO (_: string) : 'a =
    raise Fail "NilpotentInduction: not yet ported"
end

