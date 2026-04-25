use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/Dirac.sml

  Purpose
  - Placeholder SML translation of `atlas-scripts/Dirac.at`.
  - The `.at` script contains Dirac-cohomology and Dirac-inequality utilities
    used in various unitary-dual computations.

  Status
  - Not yet implemented in SML as a standalone library module.
  - For the F4/FPP verifier, the relevant functionality is implemented in:
      - `atlas-scripts-sml/FPP_globalDirac.sml`
      - `atlas-scripts-sml/FPP_localDirac.sml`
    which are tailored to the verification pipeline and already use the Atlas
    C++ primitives exposed via the Poly/ML FFI.
*)

structure Dirac = struct
  type group = AtlasFFI.group
  type param = AtlasFFI.param

  fun check (_: group, _: param) : unit =
    raise Fail "Dirac.check: not yet ported (use FPP_*Dirac modules for current verifier)"
end

