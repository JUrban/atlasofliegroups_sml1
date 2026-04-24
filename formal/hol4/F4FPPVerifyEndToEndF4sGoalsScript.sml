(*
  File: formal/hol4/F4FPPVerifyEndToEndF4sGoalsScript.sml

  Purpose
  - State and prove (by composition) the end-to-end theorem we ultimately want
    for the *concrete* target group `F4s`.

  What this theorem means
  - Under the abstract bridge predicates:
      - `fast_program_succeeds F4s dirac`
      - `slow_program_succeeds F4s`
    we can derive:
      - `U_slow F4s (D_slow F4s) = U_fast F4s`
      - and the bottom-layer postcondition `bottom_layer_total_ok`.

  Status
  - This theory is “OK” as a logical composition.
  - It is still *CHEAT-tainted overall* because the bridge lemmas (program
    success ⇒ obligations) are currently `cheat`ed in earlier theories.
*)

open HolKernel Parse boolLib bossLib;

open pred_setTheory pred_setLib;

open F4FPPVerifyTargetGroupGoalsTheory;
open F4FPPVerifySMLBridgeGoalsTheory;
open F4FPPVerifyFastProgramSplitBridgeGoalsTheory;
open F4FPPVerifySlowBridgeDetailedGoalsTheory;
open F4FPPVerifyRefinedMainGoalsTheory;
open F4FPPVerifySpecTheory;
open F4FPPBottomLayerGoalsTheory;

val _ = new_theory "F4FPPVerifyEndToEndF4sGoals";

Theorem fast_and_slow_programs_succeed_gives_equivalence_F4s:
  !dirac.
    fast_program_succeeds F4s dirac /\ slow_program_succeeds F4s ==>
      U_slow F4s (D_slow F4s) = U_fast F4s /\
      bottom_layer_total_ok F4s dirac (U_fast F4s)
Proof
  rpt strip_tac
  \\ drule fast_program_succeeds_imp_refined_fast_obligations_F4s
  \\ drule slow_program_succeeds_imp_refined_slow_obligations_detailed
  \\ metis_tac[refined_obligations_imply_equivalence]
QED

(* The scripts set the Dirac flag, so this is the concrete specialization. *)
Theorem fast_and_slow_programs_succeed_gives_equivalence_F4s_dirac:
  fast_program_succeeds F4s T /\ slow_program_succeeds F4s ==>
    U_slow F4s (D_slow F4s) = U_fast F4s /\
    bottom_layer_total_ok F4s T (U_fast F4s)
Proof
  metis_tac[fast_and_slow_programs_succeed_gives_equivalence_F4s]
QED

val _ = export_theory ();

