(*
  File: formal/hol4/F4FPPVerifyParamHashBridgeStateBuildTraceStoresTraceDecomposeCheatsGoalsScript.sml

  Purpose
  - Provide the CHEAT-tainted composition that turns the trace-level stores
    bundle into the existing extensional ParamHash obligations (modulo `atlas_eq`).

  Main statement
  - Under the Atlas hash/equality contract `atlas_hash_eq_ok`, the trace-level
    bundle
      `paramhash_obligations_build_state_trace_stores_factored_atlas_eq`
    implies the extensional ParamHash obligations
      `paramhash_obligations_factored_atlas_eq`.

  Status
  - This file introduces no new `cheat`, but it depends on the currently
    CHEAT-tainted build-set/upgrade lemmas used to relate `set(trace)` to
    `ph_set(build_state)`.
*)

open HolKernel Parse boolLib bossLib;

open F4FPPVerifyAtlasFFIContractsGoalsTheory;
open F4FPPVerifyParamHashBridgeDecomposeGoalsTheory;
open F4FPPVerifyParamHashBridgeStateBuildTraceStoresDecomposeGoalsTheory;
open F4FPPVerifyParamHashBridgeStateBuildTraceStoresTraceCheatsGoalsTheory;
open F4FPPVerifyParamHashBridgeStateBuildTraceStoresTraceDecomposeGoalsTheory;

val _ = new_theory "F4FPPVerifyParamHashBridgeStateBuildTraceStoresTraceDecomposeCheatsGoals";

Theorem atlas_hash_eq_ok_and_build_state_trace_stores_factored_atlas_eq_imp_paramhash_obligations_factored_atlas_eq:
  !g.
    atlas_hash_eq_ok /\
    paramhash_obligations_build_state_trace_stores_factored_atlas_eq g ==>
      paramhash_obligations_factored_atlas_eq g
Proof
  rpt strip_tac
  \\ `fast_param_set_is_paramhash g` by
       metis_tac[build_state_trace_stores_factored_atlas_eq_imp_fast_param_set_is_paramhash]
  \\ `paramhash_build_state_ok g` by
       metis_tac[build_state_trace_stores_factored_atlas_eq_imp_paramhash_build_state_ok]
  \\ `paramhash_build_ps_stores_U_fast_atlas_eq g` by
       metis_tac[build_state_trace_stores_factored_atlas_eq_imp_paramhash_build_ps_stores_U_fast_atlas_eq]
  \\ `paramhash_build_m_ok g` by
       metis_tac[paramhash_build_state_ok_imp_paramhash_build_m_ok]
  \\ `paramhash_build_stores_U_fast_atlas_eq g` by
       metis_tac[atlas_hash_eq_ok_and_paramhash_build_m_ok_and_build_ps_stores_imp_build_stores]
  \\ match_mp_tac atlas_hash_eq_ok_and_build_state_stores_factored_atlas_eq_imp_paramhash_obligations_factored_atlas_eq
  \\ fs[paramhash_obligations_build_state_stores_factored_atlas_eq_def]
QED

val _ = export_theory ();

