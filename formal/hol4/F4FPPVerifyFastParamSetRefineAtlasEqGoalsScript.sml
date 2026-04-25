(*
  File: formal/hol4/F4FPPVerifyFastParamSetRefineAtlasEqGoalsScript.sml

  Purpose
  - Refine `fast_param_set_ok_atlas_eq` into smaller obligations that match the
    SML interface:
      - `ParamHash.list` provides a representative list (`fast_list g`), and
      - `ParamHash.contains` tests membership modulo Atlas equality `atlas_eq`.

  What this theory adds
  - `fast_param_set_contains_ok_atlas_eq g`:
      `ps_contains (fast_param_set g) p ⇔ mem_set_atlas_eq p (U_fast g)`.
  - `fast_param_set_rep_ok_atlas_eq g`:
      combines the existing list obligation `fast_param_set_list_ok g` with the
      new modulo-`atlas_eq` contains obligation.

  Status
  - “OK”: definitional unfolding and set/list reasoning only.
*)

open HolKernel Parse boolLib bossLib;

open pred_setTheory pred_setLib;

open F4FPPVerifyGoalsTheory;
open F4FPPVerifyAtlasEqSetGoalsTheory;

open F4FPPBottomLayerParamSetAtlasEqGoalsTheory;
open F4FPPVerifyFastParamSetGoalsTheory;
open F4FPPVerifyFastParamSetRefineGoalsTheory;
open F4FPPVerifyFastParamSetAtlasEqGoalsTheory;

val _ = new_theory "F4FPPVerifyFastParamSetRefineAtlasEqGoals";

Definition fast_param_set_contains_ok_atlas_eq_def:
  fast_param_set_contains_ok_atlas_eq g <=>
    !p. ps_contains (fast_param_set g) p <=> mem_set_atlas_eq p (U_fast g)
End

Definition fast_param_set_rep_ok_atlas_eq_def:
  fast_param_set_rep_ok_atlas_eq g <=>
    fast_param_set_list_ok g /\ fast_param_set_contains_ok_atlas_eq g
End

Theorem fast_param_set_rep_ok_atlas_eq_imp_fast_param_set_ok_atlas_eq:
  !g. fast_param_set_rep_ok_atlas_eq g ==> fast_param_set_ok_atlas_eq g
Proof
  rw[ fast_param_set_rep_ok_atlas_eq_def
    , fast_param_set_ok_atlas_eq_def
    , fast_param_set_list_ok_def
    , fast_param_set_contains_ok_atlas_eq_def
    , param_set_rep_ok_atlas_eq_def
    , U_fast_def
    ]
QED

val _ = export_theory ();

