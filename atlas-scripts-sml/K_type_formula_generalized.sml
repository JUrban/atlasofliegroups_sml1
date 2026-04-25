use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/Split.sml";
use "atlas-scripts-sml/ParamPol.sml";
use "atlas-scripts-sml/KTypePol.sml";

(*
  File: atlas-scripts-sml/K_type_formula_generalized.sml

  Purpose
  - Partial SML translation of `atlas-scripts/K_type_formula_generalized.at`.
  - The `.at` file provides two kinds of utilities:
      (1) “generalized” K-type-formula combinators based on highest-weight
          ladders (requires `KHighestWeight` and `highest_weight@KType`), and
      (2) small truncation helpers for `ParamPol` and `KTypePol`.

  Implemented in this port
  - `truncate_parampol(P,n)`:
      keep only those `ParamPol` terms whose parameter height `<= n`.
      This is the SML analogue of:
        `truncate(ParamPol P,int n)` in the `.at` file.
  - `cut_ktypepol(P,n)`:
      return the `KTypePol` truncated by height, using the existing FFI helper
      `atlas_ktypepol_to_ht` (wrapped as `KTypePol.toHT`).
      This corresponds to `.at`’s `cut(KTypePol P,int n)`.

  Not yet implemented (documented stubs)
  - `ladder`, `K_type_formula` and the `K_type_formula_ladder` wrappers.
    These require `K.at`’s `KHighestWeight` datatype and `highest_weight@KType`,
    plus a faithful SML replacement for `.at`’s `K_type` / `k_highest_weight`.

  Ownership / memory
  - `truncate_parampol` returns a new `ParamPol.t` owning freshly cloned
    parameters. Callers must `ParamPol.free` it when done.
  - `cut_ktypepol` returns a new owned `KTypePol` handle. Callers must free it
    with `KTypePol.free`.
*)

structure K_type_formula_generalized = struct
  type group = AtlasFFI.group
  type param = AtlasFFI.param
  type ktypepol = AtlasFFI.ktypepol

  fun truncate_parampol (P: ParamPol.t, n: int) : ParamPol.t =
    let
      val out = ParamPol.create ()
      fun keep (c, p) =
        if AtlasFFI.atlas_param_height p <= n then
          ParamPol.addTermMove (out, c, ParamPol.cloneParam p)
        else
          ()
    in
      List.app keep (ParamPol.terms P);
      out
    end

  fun cut_ktypepol (P: ktypepol, n: int) : ktypepol =
    KTypePol.toHT (P, n)

  (* --- Stubs for the highest-weight ladder machinery --- *)

  fun ladder (_: group, _: AtlasFFI.ktype) : int -> ktypepol =
    raise Fail "K_type_formula_generalized.ladder: not yet ported (requires KHighestWeight/highest_weight)"

  fun K_type_formula (_: group, _: int -> ktypepol, _: int) : ktypepol =
    raise Fail "K_type_formula_generalized.K_type_formula: not yet ported (requires ladder + K_type_formula@KType)"

  fun K_type_formula_ladder (_: AtlasFFI.ktype, _: int) : ktypepol =
    raise Fail "K_type_formula_generalized.K_type_formula_ladder: not yet ported"

  fun K_type_formula_ladder_truncate (_: AtlasFFI.ktype, _: int, _: int) : ktypepol =
    raise Fail "K_type_formula_generalized.K_type_formula_ladder_truncate: not yet ported"
end
