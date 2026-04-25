use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/exceptionalData.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/exceptionalData.at`.
  - The `.at` script provides assorted exceptional-type data tables used by
    other scripts (nilpotent orbits, packets, etc.).

  Status
  - Not yet ported: this is largely data-driven and depends on the corresponding
    exceptional-type infrastructure in the interpreter.
*)

structure ExceptionalData = struct
  fun TODO (_: string) : 'a =
    raise Fail "ExceptionalData: not yet ported"
end

