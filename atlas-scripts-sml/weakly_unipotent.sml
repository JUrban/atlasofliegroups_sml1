use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/weakly_unipotent.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/weakly_unipotent.at`.
  - The `.at` script defines a heuristic predicate `is_weakly_unipotent(p)`
    based on:
      - enumerating candidate infinitesimal characters near `p.gamma`,
      - checking non-vanishing of translated character formulas at those
        candidates (`T_param(character_formula(p),mu)`).

  Status
  - Not yet implemented in SML:
      - `kl.at` / `character_formula` are not yet ported.
      - The “translation functor” layer `translate.at` (in the sense used by
        `weakly_unipotent.at`) is not yet available beyond basic parameter
        translation helpers.
      - Weyl-group enumeration and the `fundamental_weights` API are not yet
        exposed in the required form.
*)

structure Weakly_unipotent = struct
  type group = AtlasFFI.group
  type param = AtlasFFI.param

  fun is_weakly_unipotent (p: param) : bool =
    let
      val _ = p
    in
    raise Fail "Weakly_unipotent.is_weakly_unipotent: not yet ported"
    end
end
