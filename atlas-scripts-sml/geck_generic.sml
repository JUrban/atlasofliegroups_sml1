use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/geck_generic.sml

  Purpose
  - Translation scaffold for `atlas-scripts/geck_generic.at`.
  - The `.at` script relates to generic degrees / Hecke-algebra data; see the
    original for details.

  Status
  - Not yet ported: this file currently defines only a placeholder structure.
*)

structure Geck_generic = struct
  fun TODO (_: string) : 'a =
    raise Fail "Geck_generic: not yet ported"
end

