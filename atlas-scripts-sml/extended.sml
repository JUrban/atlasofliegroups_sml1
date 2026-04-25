use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/extended.sml

  Purpose
  - Translation scaffold for `atlas-scripts/extended.at`.
  - The `.at` script provides extended-group helpers (extended parameters,
    Cayley/cross actions, and related utilities); see the original for details.

  Status
  - Not yet ported as a standalone API surface in SML: use the existing partial
    ports (`extended_types.sml`, `extended_misc.sml`, `extended_cross.sml`, …).
*)

structure Extended = struct
  fun TODO (_: string) : 'a =
    raise Fail "Extended: not yet ported"
end

