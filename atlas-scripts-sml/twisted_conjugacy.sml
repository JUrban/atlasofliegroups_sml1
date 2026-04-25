use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/twisted_conjugacy.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/twisted_conjugacy.at`.
  - The `.at` script decides conjugacy of (twisted) semisimple elements using
    affine Weyl-group “co-dominant” normalization (`affine_co_make_dominant`)
    and lattice tests.

  Status
  - Not yet ported: the current SML `Affine` port is intentionally minimal and
    does not implement `affine_co_make_dominant` (the key algorithmic step).
  - Once an SML implementation of affine co-dominant normalization is added
    (or exposed via the FFI), this script can be ported in a mostly direct way.
*)

structure Twisted_conjugacy = struct
  fun TODO (_: string) : 'a =
    raise Fail "Twisted_conjugacy: not yet ported"
end

