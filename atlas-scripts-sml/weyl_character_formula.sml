use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/weyl_character_formula.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/weyl_character_formula.at`.
  - The `.at` script implements evaluation/traces of Weyl characters, including
    `trace_strong_real`, used by `finite_dimensional_signature.at`.

  Status
  - Not yet ported: requires substantial Weyl character computation and
    cyclotomic/torus-element evaluation infrastructure not currently present in SML.
*)

structure Weyl_character_formula = struct
  fun TODO (_: string) : 'a =
    raise Fail "Weyl_character_formula: not yet ported"
end

