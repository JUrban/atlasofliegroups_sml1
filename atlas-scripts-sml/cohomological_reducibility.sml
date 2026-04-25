use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/cohomological_reducibility.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/cohomological_reducibility.at`.
  - The `.at` script contains reducibility tests related to cohomological
    induction.

  Status
  - Not yet ported: cohomological induction is not currently implemented in SML.
*)

structure Cohomological_reducibility = struct
  fun TODO (_: string) : 'a =
    raise Fail "Cohomological_reducibility: not yet ported"
end

