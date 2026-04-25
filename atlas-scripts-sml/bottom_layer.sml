use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/bottom_layer.sml

  Purpose
  - Placeholder SML translation of `atlas-scripts/bottom_layer.at`.
  - The `.at` file is a large driver implementing “bottom layer” checks and
    induction-based diagnostics (Dirac, theta-stable parabolics, K-signatures,
    etc.).

  Status
  - Not yet ported as a standalone SML module. For the F4/FPP verifier, the
    relevant bottom-layer logic is implemented in:
      - `atlas-scripts-sml/FPP_globalDirac.sml`
    which is what `VerifyF4FPP.sml` uses.
  - A future port of `bottom_layer.at` would largely be a matter of wiring
    together already-ported subroutines plus adding missing induction/nilpotent
    components.
*)

structure BottomLayer = struct
  fun run (_: AtlasFFI.group) : unit =
    raise Fail "BottomLayer.run: not yet ported (use FPP_globalDirac for current verifier)"
end

