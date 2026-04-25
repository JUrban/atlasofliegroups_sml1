use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/ParamPol.sml";
use "atlas-scripts-sml/Split.sml";

(*
  File: atlas-scripts-sml/GK_dimension.sml

  Purpose
  - Partial SML translation of `atlas-scripts/GK_dimension.at`.
  - The `.at` script provides heuristics for estimating Gelfand–Kirillov (GK)
    dimension from growth of K-types up to height bounds, along with a few small
    utility functions for truncating parameter polynomials by height.

  Implemented in this port
  - `truncate_parampol(P,n)`:
      keep only terms `p` with `height(p) <= n`, preserving coefficients.
      This corresponds to `.at`:
        `truncate(ParamPol P,int n)`.
  - `slice_parampol(P,n)`:
      keep only terms `p` with `height(p) = n`, preserving coefficients.
      This corresponds to `.at`:
        `slice(ParamPol P,int n)`.

  Not yet implemented
  - The GK-growth estimators (`growth`, `growth_std`, `growth_irr`, ...) depend
    on being able to compute dimensions of `KTypePol`/`ParamPol` objects:
      - `dimension(KTypePol)` / `dimension(ParamPol)` and `branch_std(p,n)`
    Those are currently not exposed via the SML FFI in this repository.
*)

structure GK_dimension = struct
  type param = AtlasFFI.param

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

  fun slice_parampol (P: ParamPol.t, n: int) : ParamPol.t =
    let
      val out = ParamPol.create ()
      fun keep (c, p) =
        if AtlasFFI.atlas_param_height p = n then
          ParamPol.addTermMove (out, c, ParamPol.cloneParam p)
        else
          ()
    in
      List.app keep (ParamPol.terms P);
      out
    end

  (* Stubs mirroring the `.at` entry points (kept for compatibility). *)
  fun dim_K_types_std_upto (_: param, _: int) : int =
    raise Fail "GK_dimension.dim_K_types_std_upto: not yet ported (needs KTypePol dimension + branch_std(Param))"
end
