use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/ParamPol.sml";
use "atlas-scripts-sml/Split.sml";

(*
  File: atlas-scripts-sml/coherent.sml

  Purpose
  - Partial SML translation of `atlas-scripts/coherent.at`.
  - The `.at` script implements coherent continuation operators and related
    translation-functor machinery; it is a major dependency for many higher
    level scripts.

  Current scope
  - Implemented:
      - `Cayley_sum(k,p)` returning a `ParamPol.t` with one or two terms.
  - Not yet implemented:
      - The coherent continuation action (`coherent_std`, `coherent_irr`, etc.)
      - Translation functors `T@(Param(Pol),ratvec)`
      - Any KL/composition-series functionality
*)

structure Coherent = struct
  type param = AtlasFFI.param
  type poly = ParamPol.t

  fun failFFI (where': string) : 'a =
    raise Fail ("Coherent." ^ where' ^ ": " ^ AtlasFFI.atlas_last_error ())

  fun paramCross (p: param, s: int) : param =
    let
      val q = AtlasFFI.atlas_param_cross (p, s)
    in
      if q = Foreign.Memory.null then failFFI "cross" else q
    end

  fun paramCayley (p: param, s: int) : param =
    let
      val q = AtlasFFI.atlas_param_cayley (p, s)
    in
      if q = Foreign.Memory.null then failFFI "Cayley" else q
    end

  (* Cayley transform as a sum of one or two terms (mirrors `Cayley_sum` in `.at`). *)
  fun Cayley_sum (k: int, p: param) : poly =
    let
      val c1 = paramCayley (p, k)
      val c2 = paramCross (c1, k)
      val out = ParamPol.create ()
    in
      if AtlasFFI.atlas_param_equal (c1, c2) = 1 then
        (AtlasFFI.atlas_param_free c2;
         ParamPol.addTermMove (out, Split.one, c1);
         out)
      else
        (ParamPol.addTermMove (out, Split.one, c1);
         ParamPol.addTermMove (out, Split.one, c2);
         out)
    end

  fun coherent_std (p: param, s: int) : poly =
    let
      val _ = (p, s)
    in
      raise Fail "Coherent.coherent_std: not yet ported"
    end
end
