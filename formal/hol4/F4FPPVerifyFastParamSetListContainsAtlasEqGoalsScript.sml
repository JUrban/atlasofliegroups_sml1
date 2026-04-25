(*
  File: formal/hol4/F4FPPVerifyFastParamSetListContainsAtlasEqGoalsScript.sml

  Purpose
  - Provide small “glue” lemmas connecting:
      (1) list sound/complete obligations for `fast_param_set`, and
      (2) modulo-`atlas_eq` sound/complete obligations for `contains`,
    to the full representation predicate `fast_param_set_ok_atlas_eq`.

  Why this is a separate theory
  - `F4FPPVerifyFastParamSetAtlasEqGoalsTheory` defines `fast_param_set_ok_atlas_eq`
    and derives bottom-layer consequences.
  - `F4FPPVerifyFastParamSetRefineAtlasEqGoalsTheory` refines the `contains`
    obligations modulo `atlas_eq` and already depends on the definition above.
  - Putting the “combine list + contains-atlas_eq ⇒ ok_atlas_eq” lemma here
    avoids a dependency cycle between those theories.

  Status
  - “OK”: definitional unfolding and set/list reasoning only.
*)

open HolKernel Parse boolLib bossLib;

open pred_setTheory pred_setLib;

open F4FPPVerifyGoalsTheory;
open F4FPPVerifyFastParamSetGoalsTheory;

open F4FPPBottomLayerParamSetAtlasEqGoalsTheory;
open F4FPPVerifyFastParamSetAtlasEqGoalsTheory;
open F4FPPVerifyFastParamSetListRefineGoalsTheory;
open F4FPPVerifyFastParamSetRefineAtlasEqGoalsTheory;
open F4FPPVerifyFastParamSetContainsRefineAtlasEqGoalsTheory;

val _ = new_theory "F4FPPVerifyFastParamSetListContainsAtlasEqGoals";

Theorem fast_param_set_list_and_contains_obligations_imp_fast_param_set_ok_atlas_eq:
  !g.
    fast_param_set_list_sound g /\
    fast_param_set_list_complete g /\
    fast_param_set_contains_sound_atlas_eq g /\
    fast_param_set_contains_complete_atlas_eq g ==>
      fast_param_set_ok_atlas_eq g
Proof
  rpt strip_tac
  \\ simp[fast_param_set_ok_atlas_eq_def, param_set_rep_ok_atlas_eq_def]
  \\ conj_tac
  >- (
    `fast_param_set_setview_ok g` by
      metis_tac[fast_param_set_list_sound_and_complete_imp_setview_ok]
    \\ fs[fast_param_set_setview_ok_def]
    \\ metis_tac[]
  )
  \\ (
    `fast_param_set_contains_ok_atlas_eq g` by
      metis_tac[fast_param_set_contains_sound_and_complete_atlas_eq_imp_contains_ok_atlas_eq]
    \\ fs[fast_param_set_contains_ok_atlas_eq_def]
  )
QED

val _ = export_theory ();
