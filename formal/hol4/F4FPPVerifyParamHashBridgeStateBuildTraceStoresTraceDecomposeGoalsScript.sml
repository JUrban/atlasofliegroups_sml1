(*
  File: formal/hol4/F4FPPVerifyParamHashBridgeStateBuildTraceStoresTraceDecomposeGoalsScript.sml

  Purpose
  - Introduce a ParamHash obligation bundle that uses the canonical trace-based
    build model (`paramhash_build_state_ok g`) together with the *trace-level*
    stores predicate `paramhash_build_ps_stores_U_fast_atlas_eq g`.

  Motivation
  - The state-based stores predicate (`paramhash_build_stores_U_fast_atlas_eq g`)
    talks about `ph_set (paramhash_build_state g)`.
  - For translator/CakeML proofs it may be simpler to establish that the trace
    list `paramhash_build_ps g` represents `U_fast g` (modulo `atlas_eq`), and
    then upgrade to the state-based predicate via a pure build lemma.

  Status
  - OK: definitions and eliminators only.
*)

open HolKernel Parse boolLib bossLib;

open F4FPPVerifyParamHashBridgeGoalsTheory;
open F4FPPVerifyParamHashBridgeStateBuildTraceGoalsTheory;
open F4FPPVerifyParamHashBridgeStateBuildTraceRefineGoalsTheory;
open F4FPPVerifyParamHashBridgeStateBuildTraceStoresGoalsTheory;

val _ = new_theory "F4FPPVerifyParamHashBridgeStateBuildTraceStoresTraceDecomposeGoals";

Definition paramhash_obligations_build_state_trace_stores_factored_atlas_eq_def:
  paramhash_obligations_build_state_trace_stores_factored_atlas_eq g <=>
    fast_param_set_is_paramhash g /\
    paramhash_build_state_ok g /\
    paramhash_build_ps_stores_U_fast_atlas_eq g
End

Theorem build_state_trace_stores_factored_atlas_eq_imp_fast_param_set_is_paramhash:
  !g.
    paramhash_obligations_build_state_trace_stores_factored_atlas_eq g ==>
      fast_param_set_is_paramhash g
Proof
  simp[paramhash_obligations_build_state_trace_stores_factored_atlas_eq_def]
QED

Theorem build_state_trace_stores_factored_atlas_eq_imp_paramhash_build_state_ok:
  !g.
    paramhash_obligations_build_state_trace_stores_factored_atlas_eq g ==>
      paramhash_build_state_ok g
Proof
  simp[paramhash_obligations_build_state_trace_stores_factored_atlas_eq_def]
QED

Theorem build_state_trace_stores_factored_atlas_eq_imp_paramhash_build_ps_stores_U_fast_atlas_eq:
  !g.
    paramhash_obligations_build_state_trace_stores_factored_atlas_eq g ==>
      paramhash_build_ps_stores_U_fast_atlas_eq g
Proof
  simp[paramhash_obligations_build_state_trace_stores_factored_atlas_eq_def]
QED

Theorem paramhash_build_state_ok_imp_paramhash_build_m_ok:
  !g. paramhash_build_state_ok g ==> paramhash_build_m_ok g
Proof
  simp[paramhash_build_state_ok_def, paramhash_build_m_ok_def]
QED

val _ = export_theory ();

