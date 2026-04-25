use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/extKLbug.sml

  Purpose
  - SML placeholder for `atlas-scripts/extKLbug.at`.
  - The `.at` file is a debugging/repro script describing a suspected extended
    KL / hermitian-form inconsistency for certain E6 examples.

  Status
  - Not ported: it depends on hermitian form computation, branching of standard
    modules, and `convert_cform_hermitian` for KTypePols, which are only
    partially available in the current SML port.
*)

structure ExtKLbug = struct
  fun TODO (_: string) : 'a =
    raise Fail "ExtKLbug: not yet ported"
end

