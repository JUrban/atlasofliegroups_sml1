(*
  File: formal/hol4/F4FPPVerifyParamHashBridgeStateBuildTraceStoresTraceToStateGoalsScript.sml

  Purpose
  - Provide an “adapter” lemma that lets the proof stack consume the
    trace-level canonical-build bundle
      `paramhash_obligations_build_state_trace_stores_factored_atlas_eq g`
    at the existing interface expected by the end-to-end obligation stack,
    namely `paramhash_obligations_state_factored_atlas_eq g`.

  Why this matters
  - The trace-level route is often the most natural translator/CakeML target:
      show `U_fast g ~ set (paramhash_build_ps g)`.
  - The end-to-end stack currently expects the state-level bundle
    `paramhash_obligations_state_factored_atlas_eq g` (wiring + `paramhash_state_ok`
    + `paramhash_stores_U_fast_atlas_eq g`).
  - Under `atlas_hash_eq_ok`, the canonical build-state model is a valid
    `paramhash_state_ok` witness, and the pure build-set lemma upgrades
    trace-level stores into the list-based stores predicate required by the
    state-level bundle.

  Status
  - OK (no `cheat`): purely logical composition on top of already-recorded
    definitions and previously proved pure-state lemmas.
*)

open HolKernel Parse boolLib bossLib;

open F4FPPVerifyAtlasFFIContractsGoalsTheory;
open F4FPPVerifyParamHashBridgeStateDecomposeGoalsTheory;
open F4FPPVerifyParamHashBridgeStateBuildTraceConsequencesGoalsTheory;
open F4FPPVerifyParamHashBridgeStateBuildTraceGoalsTheory;
open F4FPPVerifyParamHashBridgeStateBuildTraceRefineGoalsTheory;
open F4FPPVerifyParamHashBridgeStateBuildTraceStoresGoalsTheory;
open F4FPPVerifyParamHashBridgeStateBuildTraceStoresTraceCheatsGoalsTheory;
open F4FPPVerifyParamHashBridgeStateBuildTraceStoresTraceDecomposeGoalsTheory;

val _ = new_theory "F4FPPVerifyParamHashBridgeStateBuildTraceStoresTraceToStateGoals";

Theorem atlas_hash_eq_ok_and_build_state_trace_stores_factored_atlas_eq_imp_paramhash_obligations_state_factored_atlas_eq:
  !g.
    atlas_hash_eq_ok /\
    paramhash_obligations_build_state_trace_stores_factored_atlas_eq g ==>
      paramhash_obligations_state_factored_atlas_eq g
Proof
  rpt strip_tac
  \\ rw[paramhash_obligations_state_factored_atlas_eq_def]
  >- metis_tac[build_state_trace_stores_factored_atlas_eq_imp_fast_param_set_is_paramhash]
  >- (
    match_mp_tac atlas_hash_eq_ok_and_paramhash_build_state_ok_imp_paramhash_state_ok
    \\ metis_tac[build_state_trace_stores_factored_atlas_eq_imp_paramhash_build_state_ok])
  \\ (* Stores-U-fast: trace-stores -> build-stores -> list-stores. *)
  qabbrev_tac `H = paramhash_obligations_build_state_trace_stores_factored_atlas_eq g`
  \\ `paramhash_build_state_ok g` by
       metis_tac[build_state_trace_stores_factored_atlas_eq_imp_paramhash_build_state_ok, Abbr`H`]
  \\ `paramhash_build_ps_stores_U_fast_atlas_eq g` by
       metis_tac[build_state_trace_stores_factored_atlas_eq_imp_paramhash_build_ps_stores_U_fast_atlas_eq, Abbr`H`]
  \\ `paramhash_build_m_ok g` by fs[paramhash_build_state_ok_def, paramhash_build_m_ok_def]
  \\ `paramhash_build_stores_U_fast_atlas_eq g` by
       metis_tac[atlas_hash_eq_ok_and_paramhash_build_m_ok_and_build_ps_stores_imp_build_stores]
  \\ metis_tac[paramhash_build_state_ok_and_build_stores_imp_paramhash_stores_U_fast_atlas_eq]
QED

val _ = export_theory ();
