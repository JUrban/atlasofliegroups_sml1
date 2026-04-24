(*
  File: formal/hol4/F4FPPVerifyFastParamSetContainsRefineGoalsScript.sml

  Purpose
  - Refine the single obligation `fast_param_set_contains_ok g` (from
    `F4FPPVerifyFastParamSetRefineGoalsTheory`) into two one-way obligations:

      - `fast_param_set_contains_sound g`:
          if the SML `contains` returns true, the element is in the abstract set.
      - `fast_param_set_contains_complete g`:
          if an element is in the abstract set, `contains` returns true.

  Motivation
  - This matches how we will eventually justify `ParamHash.contains`:
      - soundness usually requires only a basic invariant (no bogus positives),
      - completeness typically requires stronger invariants plus the
        hash/eq-coherence assumptions (no false negatives).

  Status
  - “OK”: purely logical decomposition and recombination.
*)

open HolKernel Parse boolLib bossLib;

open pred_setTheory pred_setLib;

open F4FPPVerifyGoalsTheory;
open F4FPPVerifyFastParamSetRefineGoalsTheory;

val _ = new_theory "F4FPPVerifyFastParamSetContainsRefineGoals";

Definition fast_param_set_contains_sound_def:
  fast_param_set_contains_sound g <=>
    !p. ps_contains (fast_param_set g) p ==> p IN U_fast g
End

Definition fast_param_set_contains_complete_def:
  fast_param_set_contains_complete g <=>
    !p. p IN U_fast g ==> ps_contains (fast_param_set g) p
End

Theorem fast_param_set_contains_sound_and_complete_imp_contains_ok:
  !g.
    fast_param_set_contains_sound g /\ fast_param_set_contains_complete g ==>
      fast_param_set_contains_ok g
Proof
  rw[fast_param_set_contains_sound_def,
     fast_param_set_contains_complete_def,
     fast_param_set_contains_ok_def]
  \\ eq_tac \\ metis_tac[]
QED

val _ = export_theory ();
