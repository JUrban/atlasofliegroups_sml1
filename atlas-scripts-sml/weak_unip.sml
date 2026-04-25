use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/weak_unip.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/weak_unip.at`.
  - The `.at` script contains heuristics/reports around “weakly unipotent”
    representations.

  Status
  - Not yet ported: depends on `kl.at` character formulas and translation
    functors, which are not yet available in the SML layer.
*)

structure Weak_unip = struct
  type param = AtlasFFI.param
  fun TODO (_: string) : 'a = raise Fail "Weak_unip: not yet ported"
end

