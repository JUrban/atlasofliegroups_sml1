(*
  File: formal/hol4/F4FPPVerifyParamHashBridgeStateBuildTraceStoresGoalsScript.sml

  Purpose
  - Refine the “stores `U_fast`” obligation so it talks about the *canonical*
    trace-based build model state directly, rather than only about the observed
    list `paramhash_list g`.

  Motivation
  - The earlier predicate `paramhash_stores_U_fast_atlas_eq g` is phrased as:
      `set_atlas_eq (U_fast g) (set (paramhash_list g))`.
  - For translator/CakeML proofs we expect to obtain a stronger artefact:
      a canonical pure state `paramhash_build_state g` that models the actual
      table, plus a proof that the concrete observations agree with it.
  - With that in place, it is natural to express the “stores-U-fast” part as an
    equality between `U_fast g` and the set view of the canonical state:
      `ph_set (paramhash_build_state g)`.

  Status
  - This file is “OK”: definitions + logical implication lemmas only.
*)

open HolKernel Parse boolLib bossLib;

open pred_setTheory pred_setLib;

open F4FPPVerifyAtlasEqSetGoalsTheory;
open F4FPPVerifyGoalsTheory;
open F4FPPVerifyParamHashBridgeDecomposeGoalsTheory;
open F4FPPVerifyParamHashStateGoalsTheory;
open F4FPPVerifyParamHashBridgeStateBuildTraceGoalsTheory;

val _ = new_theory "F4FPPVerifyParamHashBridgeStateBuildTraceStoresGoals";

(* “The canonical build-state model stores `U_fast`, modulo `atlas_eq`.” *)
Definition paramhash_build_stores_U_fast_atlas_eq_def:
  paramhash_build_stores_U_fast_atlas_eq g <=>
    set_atlas_eq (U_fast g) (ph_set (paramhash_build_state g))
End

(* Trace-level variant: push the “stores-U-fast” obligation onto the trace list
   `paramhash_build_ps g`. A separate (currently CHEAT-tainted) lemma relates
   this to the state-based predicate via the pure build model. *)
Definition paramhash_build_ps_stores_U_fast_atlas_eq_def:
  paramhash_build_ps_stores_U_fast_atlas_eq g <=>
    set_atlas_eq (U_fast g) (set (paramhash_build_ps g))
End

Theorem paramhash_build_state_ok_and_build_stores_imp_paramhash_stores_U_fast_atlas_eq:
  !g.
    paramhash_build_state_ok g /\ paramhash_build_stores_U_fast_atlas_eq g ==>
      paramhash_stores_U_fast_atlas_eq g
Proof
  rpt strip_tac
  \\ fs[paramhash_build_state_ok_def, paramhash_build_stores_U_fast_atlas_eq_def,
        paramhash_stores_U_fast_atlas_eq_def, ph_set_def, paramhash_build_state_def]
  \\ fs[paramhash_observes_state_def]
QED

(* Conversely, the list-based stores-U-fast obligation implies the state-based
   one, assuming the observation agreement `paramhash_build_state_ok`. *)
Theorem paramhash_build_state_ok_and_paramhash_stores_U_fast_atlas_eq_imp_build_stores:
  !g.
    paramhash_build_state_ok g /\ paramhash_stores_U_fast_atlas_eq g ==>
      paramhash_build_stores_U_fast_atlas_eq g
Proof
  rpt strip_tac
  \\ fs[paramhash_build_state_ok_def, paramhash_observes_state_def]
  \\ fs[paramhash_stores_U_fast_atlas_eq_def, paramhash_build_stores_U_fast_atlas_eq_def,
        ph_set_def, paramhash_build_state_def]
  \\ qpat_x_assum `paramhash_list g = _` (fn eq =>
       qpat_x_assum `set_atlas_eq (U_fast g) (set (paramhash_list g))`
         (mp_tac o REWRITE_RULE [eq]))
  \\ simp[]
QED

val _ = export_theory ();
