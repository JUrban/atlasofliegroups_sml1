use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/synthetic.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/synthetic.at`.
  - The `.at` script defines “generalized KGB elements” and re-implements
    cross/Cayley transforms using root-datum and involution matrices, together
    with visualization helpers for blocks.

  Status
  - Not yet ported: significant parts of the generalized-KGB layer (including
    the required affine/coweight calculations and generalized conjugacy tests)
    are not currently exposed in the SML port.
*)

structure Synthetic = struct
  fun TODO (_: string) : 'a =
    raise Fail "Synthetic: not yet ported"
end

