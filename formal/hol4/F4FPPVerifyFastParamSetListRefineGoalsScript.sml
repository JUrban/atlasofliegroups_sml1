(*
  File: formal/hol4/F4FPPVerifyFastParamSetListRefineGoalsScript.sml

  Purpose
  - Refine the *list* side of the `fast_param_set` interface:
      `ps_list (fast_param_set g)` corresponds to enumerating the underlying
      `ParamHash` contents via `ParamHash.list`.

  What we need from the list operation
  - For the bottom-layer checks, it is sufficient that the list enumerates
    *exactly* the elements of the abstract set `U_fast g` (order does not
    matter at the set level).

  Accordingly, we introduce:
  - `fast_param_set_list_sound g`:
      every element in the enumerated list is in `U_fast g`.
  - `fast_param_set_list_complete g`:
      every element of `U_fast g` appears in the enumerated list.

  From these we derive the set-view equality:
    `set (ps_list (fast_param_set g)) = U_fast g`.

  We then show how to combine this list-setview property with the `contains`
  obligations (from `F4FPPVerifyFastParamSetContainsRefineGoalsTheory`) to
  obtain the full `fast_param_set_ok g` predicate.

  Status
  - “OK”: purely logical decomposition and recombination (no `cheat`).
*)

open HolKernel Parse boolLib bossLib;

open listTheory listLib;
open pred_setTheory pred_setLib;

open F4FPPVerifyGoalsTheory;
open F4FPPBottomLayerParamSetGoalsTheory;
open F4FPPVerifyFastParamSetGoalsTheory;
open F4FPPVerifyFastParamSetRefineGoalsTheory;
open F4FPPVerifyFastParamSetContainsRefineGoalsTheory;

val _ = new_theory "F4FPPVerifyFastParamSetListRefineGoals";

Definition fast_param_set_list_sound_def:
  fast_param_set_list_sound g <=>
    !p. MEM p (ps_list (fast_param_set g)) ==> p IN U_fast g
End

Definition fast_param_set_list_complete_def:
  fast_param_set_list_complete g <=>
    !p. p IN U_fast g ==> MEM p (ps_list (fast_param_set g))
End

Definition fast_param_set_setview_ok_def:
  fast_param_set_setview_ok g <=>
    set (ps_list (fast_param_set g)) = U_fast g
End

Theorem fast_param_set_list_sound_and_complete_imp_setview_ok:
  !g.
    fast_param_set_list_sound g /\ fast_param_set_list_complete g ==>
      fast_param_set_setview_ok g
Proof
  rw[fast_param_set_list_sound_def,
     fast_param_set_list_complete_def,
     fast_param_set_setview_ok_def]
  \\ simp[EXTENSION]
  \\ metis_tac[]
QED

Theorem fast_param_set_setview_ok_and_contains_ok_imp_fast_param_set_ok:
  !g.
    fast_param_set_setview_ok g /\ fast_param_set_contains_ok g ==>
      fast_param_set_ok g
Proof
  rw[fast_param_set_setview_ok_def, fast_param_set_contains_ok_def,
     fast_param_set_ok_def, param_set_rep_ok_def, U_fast_def]
QED

Theorem fast_param_set_list_and_contains_obligations_imp_fast_param_set_ok:
  !g.
    fast_param_set_list_sound g /\
    fast_param_set_list_complete g /\
    fast_param_set_contains_sound g /\
    fast_param_set_contains_complete g ==>
      fast_param_set_ok g
Proof
  rpt strip_tac
  \\ `fast_param_set_setview_ok g` by
       metis_tac[fast_param_set_list_sound_and_complete_imp_setview_ok]
  \\ `fast_param_set_contains_ok g` by
       metis_tac[fast_param_set_contains_sound_and_complete_imp_contains_ok]
  \\ metis_tac[fast_param_set_setview_ok_and_contains_ok_imp_fast_param_set_ok]
QED

val _ = export_theory ();
