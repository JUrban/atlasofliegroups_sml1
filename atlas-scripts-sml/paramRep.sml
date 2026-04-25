use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/paramRep.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/paramRep.at`.
  - The `.at` script computes the “special representation” sigma(p) of the
    integral Weyl group W_int(p) attached to a parameter p.

  Status
  - Not yet ported: depends on coherent continuation (`coherent.at`),
    extended-parameter actions (`extended_misc.at`), and hash-table utilities
    specialized to parameters in the `.at` environment.
*)

structure ParamRep = struct
  type param = AtlasFFI.param

  fun regularize (p: param) : param =
    let
      val _ = p
    in
      raise Fail "ParamRep.regularize: not yet ported"
    end

  fun Talphabetas (p: param) : unit =
    let
      val _ = p
    in
      raise Fail "ParamRep.Talphabetas: not yet ported"
    end

  fun TalphabetaRepeat (p: param) : param list =
    let
      val _ = p
    in
      raise Fail "ParamRep.TalphabetaRepeat: not yet ported"
    end
end

