use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/W_K.sml

  Purpose
  - Placeholder SML translation of `atlas-scripts/W_K.at`.
  - The `.at` script develops Weyl-group computations for the compact group `K`
    attached to a real form (and its Cartan subgroup).

  Status
  - Not yet implemented in SML. This depends heavily on the full `K.at` port
    (K-root datum construction) and on Weyl-group enumeration APIs.
*)

structure W_K = struct
  type group = AtlasFFI.group

  fun run (_: group) : unit =
    raise Fail "W_K.run: not yet ported"
end
