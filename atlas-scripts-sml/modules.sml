use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/ParamPol.sml";

(*
  File: atlas-scripts-sml/modules.sml

  Purpose
  - Partial SML translation scaffold for `atlas-scripts/modules.at`.
  - The `.at` script defines tagged wrappers for standard/irreducible modules
    and provides convenience operations in the Grothendieck group via:
      - `composition_series` (standard -> irreducible combination)
      - `character_formula` (irreducible -> standard combination)

  Status
  - Not yet implemented: the SML port currently lacks `kl.at` functionality for
    `composition_series` and `character_formula`.
  - We keep the tags and string formatting helpers because they are useful for
    future ports and debugging.
*)

structure Modules = struct
  type param = AtlasFFI.param
  type poly = ParamPol.t

  datatype tag = Std | Irr | K_types

  type tag_Param = {p: param, tag: tag}
  type tag_ParamPol = {P: poly, tag: tag}

  fun tagToString Std = "std"
    | tagToString Irr = "irr"
    | tagToString K_types = "K_types"

  fun to_str_param (p: param) : string =
    "(x=" ^ Int.toString (AtlasFFI.atlas_param_x p)
    ^ ",lambda=" ^ AtlasFFI.atlas_param_lambda_text p
    ^ ",nu=" ^ AtlasFFI.atlas_param_nu_text p
    ^ ")"

  fun to_str_std (p: param) : string = "I" ^ to_str_param p
  fun to_str_irr (p: param) : string = "J" ^ to_str_param p

  fun to_str_tagParam ({p, tag}: tag_Param) : string =
    (case tag of
       Std => to_str_std p
     | Irr => to_str_irr p
     | K_types => "J_K" ^ to_str_param p)

  fun composition_series (p: param) : poly =
    let
      val _ = p
    in
      raise Fail "Modules.composition_series: not yet ported (requires KL/composition series)"
    end

  fun character_formula (p: param) : poly =
    let
      val _ = p
    in
      raise Fail "Modules.character_formula: not yet ported (requires KL/character formulas)"
    end
end
