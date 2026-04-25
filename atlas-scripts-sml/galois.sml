use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/galois.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/galois.at`.
  - The `.at` script computes various invariants of real forms (e.g. the number
    of strong real forms, component group sizes, central invariants, and H^1).

  Status
  - Not yet ported: it depends on `square_classes` and other interpreter-side
    operations on `InnerClass`/`RealForm` that are not currently exposed via
    the SML FFI.
*)

structure Galois = struct
  fun TODO (_: string) : 'a =
    raise Fail "Galois: not yet ported"
end

