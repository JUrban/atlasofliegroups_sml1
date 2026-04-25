use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/K_Nilpotent.sml

  Purpose
  - Placeholder SML translation of `atlas-scripts/K_Nilpotent.at`.
  - The `.at` script concerns nilpotent cone / K-orbit / associated variety
    computations for K, typically relying on nilpotent-orbit data tables and
    related invariants.

  Status
  - Not yet implemented in SML because nilpotent-orbit infrastructure is not
    yet ported to `atlas-scripts-sml/`.
*)

structure K_Nilpotent = struct
  fun run (_: AtlasFFI.group) : unit =
    raise Fail "K_Nilpotent.run: not yet ported (requires nilpotent-orbit infrastructure)"
end

