use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/hodgeParamLaurentPol.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/hodgeParamLaurentPol.at`.
  - The `.at` script defines a Laurent-polynomial variant of `hodgeParamPol`,
    with coefficients in Z[v,v^{-1}].

  Status
  - Not yet ported: depends on the (missing) `hodgeParamPol` layer and on
    additional normalization/tensor routines.
*)

structure HodgeParamLaurentPol = struct
  fun TODO (_: string) : 'a =
    raise Fail "HodgeParamLaurentPol: not yet ported"
end

