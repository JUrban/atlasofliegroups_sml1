use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/hodge_functions.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/hodge_functions.at`.
  - The `.at` script is part of a larger “Hodge/Dirac” pipeline, providing
    helper functions for Hodge parameter polynomials and related operations.

  Status
  - Not yet ported: the Hodge parameter polynomial types (`hodgeParamPol`,
    `hodgeParamLaurentPol`) and their normalization/tensor routines are not yet
    implemented in the SML port.
*)

structure Hodge_functions = struct
  fun TODO (_: string) : 'a =
    raise Fail "Hodge_functions: not yet ported"
end

