use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/Lattice.sml";
use "atlas-scripts-sml/WeylWord.sml";
use "atlas-scripts-sml/cross_W_orbit.sml";

(*
  File: atlas-scripts-sml/Tensor_Products.sml

  Purpose
  - Partial SML translation of `atlas-scripts/Tensor_Products.at`.
  - The `.at` script provides K-type tensor-product helpers for certain
    groups, using cross-action transport between KGB fibers and weight shifts.

  Implemented in this port (useful, self-contained core)
  - `move_weight`:
      `.at`: `move_weight ((x,mu),y) = let w=cross_divide(y,x) in (y,w*mu)`
    In SML we expose it explicitly with the group handle:
      `move_weight (g, (x,mu), y)`.

    This is a handy primitive when you already have a `ratvec` weight `mu`
    attached to a KGB element `x` and need to transport it along the KGB cross
    graph to another element `y`.

  Not yet implemented
  - `add_weight` and the higher-level tensor-product routines depend on many
    additional `.at` components not yet ported:
      - `move_to_distinguished_fiber`, `highest_weight(s)`, `K_types`,
        `standardize`, `K_type_lambda`, `dimension`, and more.
  - `Tensor_product_Sp4` also depends on `nilpotent_orbits.at`.

  Coordinate conventions
  - `CrossWOrbit.cross_divide` returns a Weyl word `w` satisfying
      `y = cross(w, x)`  (left cross-action convention).
  - `WeylWord.actRatvec (g,w,mu)` computes `w * mu` using the Weyl action on
    weights in the group’s root datum.

  Ownership
  - This module does not allocate Atlas heap handles; it uses only integer KGB
    indices and rational vectors, plus group queries.
*)

structure Tensor_Products = struct
  type group = AtlasFFI.group
  type ratvec = Lattice.ratvec
  type weyl_word = WeylWord.t

  (* Transport a weight from KGB element `x` to `y` along the cross orbit. *)
  fun move_weight (g: group, (x: int, mu: ratvec), y: int) : int * ratvec =
    let
      val w : weyl_word = CrossWOrbit.cross_divide (g, y, x)
      val mu' = WeylWord.actRatvec (g, w, mu)
    in
      (y, mu')
    end

  (* Stubs for the remaining `.at` API. *)
  fun add_weight_ktype (_: unit) : unit =
    raise Fail "Tensor_Products.add_weight_ktype: not yet ported"

  fun Tensor_product_Sp4 (_: unit) : unit =
    raise Fail "Tensor_Products.Tensor_product_Sp4: not yet ported"
end

