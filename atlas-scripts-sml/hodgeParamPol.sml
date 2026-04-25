use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/hodgeParamPol.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/hodgeParamPol.at`.
  - The `.at` script defines the `hodgeParamPol` datatype (a ParamPol-like
    object whose coefficients are polynomials in a formal variable `v`) and
    related combinators used in the Hodge/Dirac pipelines.

  Status
  - Not yet ported: this requires a dedicated polynomial-of-parameters layer
    (distinct from `ParamPol.t`) and additional conventions about coefficient
    collection/truncation that are not implemented in the current SML port.
*)

structure HodgeParamPol = struct
  fun TODO (_: string) : 'a =
    raise Fail "HodgeParamPol: not yet ported"
end

