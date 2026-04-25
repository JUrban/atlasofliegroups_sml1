use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/smallGamma.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/smallGamma.at`.
  - The `.at` script computes small translation shifts (beta1/beta2) used to
    speed up character-formula computations by translating to a nicer
    integrality position and back.

  Status
  - Not yet ported: it depends on `FPP_faces_geom.at` (walls/fundamental alcove
    logic) and on the KL `character_formula` + translation functors.
*)

structure SmallGamma = struct
  fun TODO (_: string) : 'a =
    raise Fail "SmallGamma: not yet ported"
end

