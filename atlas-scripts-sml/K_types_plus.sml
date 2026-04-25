use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/K.sml";
use "atlas-scripts-sml/KType.sml";
use "atlas-scripts-sml/parabolics.sml";

(*
  File: atlas-scripts-sml/K_types_plus.sml

  Purpose
  - Placeholder SML translation of `atlas-scripts/K_types_plus.at`.
  - The `.at` script provides a substantial enhancement of `K_types`/`highest_weights`
    functionality and implements the algorithm `tau_q` (KHatHowe Thm 11.9 / 16.6),
    which decomposes a K-type `mu` into:
      - a theta-stable parabolic `Q`, and
      - a 1-dimensional Levi parameter `mu_L`
    such that `mu = theta_induce_standard(mu_L*0,G)` (up to standardization).

  Status
  - Not yet implemented in SML. It depends on a large collection of unported
    interpreter-level routines:
      - `parabolic_LKT`, `theta_induce_standard`, `Levi`, `x_min`, `maximal`
      - `highest_weights`, `K_types@KHighestWeight`, `R_K_dom_mu_orbit`
      - `all_G_spherical_same_differential`, `move_weight`, ...
  - The SML codebase currently has partial building blocks (`Parabolics`, some
    induction utilities, and K-type branching), but not enough to reproduce the
    `.at` algorithm faithfully.

  Intended API surface (to support downstream ports)
  - `K_types_plus_ktype(mu)` and `tau_q(mu)` are the primary entry points used by
    `K_type_formula.at`.
  - When this module is implemented, it should return *owned* `param`/`ktype`
    handles that callers must free.
*)

structure K_types_plus = struct
  type group = AtlasFFI.group
  type param = AtlasFFI.param
  type ktype = KType.ktype
  type parabolic = Parabolics.parabolic
  type KHighestWeight = K.KHighestWeight

  fun K_types_plus_highest (_: KHighestWeight) : parabolic * (param * ktype) list =
    raise Fail "K_types_plus.K_types_plus_highest: not yet ported"

  fun K_types_plus_ktype (_: ktype) : parabolic * (param * ktype) list =
    raise Fail "K_types_plus.K_types_plus_ktype: not yet ported"

  fun tau_q (_: ktype) : parabolic * param =
    raise Fail "K_types_plus.tau_q: not yet ported"
end

