use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/gl4H.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/gl4H.at`.
  - The `.at` script contains examples and computations for a specific real
    form related to GL(4, H) and its unitary dual.

  Status
  - Not yet ported: depends on `.at`-level unitary/induction/Dirac tooling that
    has not been reconstructed in the SML port.
*)

structure GL4H = struct
  type group = AtlasFFI.group
  type param = AtlasFFI.param

  fun TODO (_: string) : 'a =
    raise Fail "GL4H: not yet ported"
end

