(*
  File: formal/hol4/F4FPPVerifyGlobalDiracBridgeGoalsScript.sml

  Purpose
  - Provide a dedicated bridge layer for the `FPP_globalDirac.sml` bottom-layer
    verification pipeline.

  Why a separate bridge?
  - The fast program `VerifyF4FPP.sml` is conceptually two phases:
      (1) build the unitary parameter hash (fast semantics obligations), and
      (2) run the bottom-layer checks / rho-seeding (`FPP_globalDirac`).
  - We already have:
      - a set-level spec (`bottom_layer_total_ok`) in
        `F4FPPBottomLayerGoalsTheory`,
      - a param_set-level spec that matches SML’s interface in
        `F4FPPBottomLayerParamSetGoalsTheory`,
      - and a named param_set object `fast_param_set g` plus its representation
        obligation (`fast_param_set_ok`) in
        `F4FPPVerifyFastParamSetGoalsTheory`.
  - This theory records, top-down, the precise theorem statements we will want
    about `FPP_globalDirac` success.

  Status
  - This theory introduces the abstract success predicate
    `bottom_layer_program_succeeds`. The detailed bridge obligations and
    decomposition live in:
      - `F4FPPVerifyGlobalDiracBridgeDecomposeGoalsTheory` (OK definitions +
        recombination), and
      - `F4FPPVerifyGlobalDiracBridgeDecomposeCheatsGoalsTheory` (currently
        `cheat`ed bridge lemmas).
*)

open HolKernel Parse boolLib bossLib;

open pred_setTheory pred_setLib;

open F4FPPBottomLayerGoalsTheory;
open F4FPPBottomLayerParamSetGoalsTheory;
open F4FPPVerifyFastParamSetGoalsTheory;
open F4FPPVerifyAtlasFFIContractsGoalsTheory;
open F4FPPBottomLayerGoalsAtlasEqTheory;
open F4FPPBottomLayerParamSetAtlasEqGoalsTheory;
open F4FPPVerifyFastParamSetAtlasEqGoalsTheory;

val _ = new_theory "F4FPPVerifyGlobalDiracBridgeGoals";

(* Abstract predicate: the SML bottom-layer pipeline returned successfully for
   the fast program’s param_set (no exception raised). *)
val _ = new_constant ("bottom_layer_program_succeeds", ``:group -> bool -> bool``);
val _ = export_theory ();
