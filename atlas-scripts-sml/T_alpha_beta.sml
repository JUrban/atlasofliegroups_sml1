use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/T_alpha_beta.sml

  Purpose
  - Placeholder SML translation of `atlas-scripts/T_alpha_beta.at`.
  - The `.at` script explores operations (`tab`, `tab_orbit`) on parameters
    using `in_tau`, cross/Cayley sets, and stability infrastructure.

  Status
  - Not yet implemented in SML because it depends on `stable.at` (not ported)
    and additional ParamPol utilities.
*)

structure T_alpha_beta = struct
  type param = AtlasFFI.param

  fun tab_orbit (_: param) : AtlasFFI.param list =
    raise Fail "T_alpha_beta.tab_orbit: not yet ported (depends on stable/in_tau)"
end

