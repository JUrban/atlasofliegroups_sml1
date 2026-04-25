(*
  File: formal/hol4/F4FPPVerifyFastProgramSplitBridgeGoalsScript.sml

  Purpose
  - Decompose the fast “program success” predicate into:
      (1) success of the compute phase (`fast_compute_program_succeeds`), and
      (2) success of the bottom-layer phase (`bottom_layer_program_succeeds`),
    and provide a compositional route to the refined fast obligations.

  Motivation
  - `VerifyF4FPP.compute()` is essentially:
      computeAllIntoParamHash; then FPP_globalDirac bottom-layer checks.
  - The refined bridge stack already contains:
      - compute-phase goal theories (`F4FPPVerifyFastComputeBridgeGoalsTheory`)
      - bottom-layer bridge goal theories (`F4FPPVerifyGlobalDiracBridgeGoalsTheory`)
    but `fast_program_succeeds` (from `F4FPPVerifySMLBridgeGoalsTheory`) is a
    single abstract predicate.
  - This theory records the split explicitly so later we can replace the split
    lemma with a real connection to the SML control-flow.

  Status
  - The split lemma is `cheat`ed for now; composition lemmas are OK.
*)

open HolKernel Parse boolLib bossLib;

open pred_setTheory pred_setLib;

open F4FPPVerifySMLBridgeGoalsTheory;
open F4FPPVerifyFastComputeBridgeGoalsTheory;
open F4FPPVerifyFastComputeBridgeDecomposeGoalsTheory;
open F4FPPVerifyFastComputeBridgeDecomposeCheatsGoalsTheory;
open F4FPPVerifyGlobalDiracBridgeGoalsTheory;
open F4FPPVerifyGlobalDiracBridgeDecomposeGoalsTheory;
open F4FPPVerifyRefinedMainGoalsTheory;
open F4FPPVerifyTargetGroupGoalsTheory;

val _ = new_theory "F4FPPVerifyFastProgramSplitBridgeGoals";

(* Split bridge: success of the fast program implies success of each phase. *)
Theorem fast_program_succeeds_imp_phase_success:
  !g dirac.
    fast_program_succeeds g dirac ==>
      fast_compute_program_succeeds g /\ bottom_layer_program_succeeds g dirac
Proof
  (*
    Intended proof (later, without `cheat`):
    - unfold `VerifyF4FPP.compute` and show it calls the two phases in sequence,
      and “overall success” implies each returns without raising.
  *)
  cheat
QED

(* Composition: for the target group `F4s`, phase success implies the refined
   fast obligations used by `F4FPPVerifyRefinedMainGoalsTheory`. *)
Theorem fast_program_succeeds_imp_refined_fast_obligations_F4s:
  !dirac.
    fast_program_succeeds F4s dirac ==>
      fast_semantic_ok F4s /\
      bottom_layer_total_ok F4s dirac (U_fast F4s)
Proof
  rpt gen_tac
  \\ disch_tac
  \\ drule fast_program_succeeds_imp_phase_success
  \\ disch_then strip_assume_tac
  \\ drule fast_compute_program_succeeds_imp_fast_compute_obligations_factored
  \\ disch_then assume_tac
  \\ `fast_semantic_ok F4s` by metis_tac[fast_compute_obligations_imp_fast_semantic_ok]
  \\ `fast_param_set_ok F4s` by metis_tac[fast_compute_obligations_imp_fast_param_set_ok]
  \\ `bottom_layer_total_ok F4s dirac (U_fast F4s)` by
       metis_tac
         [ bottom_layer_program_succeeds_and_fast_param_set_ok_imp_total_ok_noncompact_decomposed
         , F4s_not_compact
         ]
  \\ simp[]
QED

val _ = export_theory ();
