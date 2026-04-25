(*
  File: formal/hol4/F4FPPVerifyParamHashBridgeStateBuildTraceRefineCheatsGoalsScript.sml

  Purpose
  - Isolate the (currently `cheat`ed) bridge theorems connecting fast compute
    phase success to the *components* of `paramhash_build_state_ok g` as
    introduced in `F4FPPVerifyParamHashBridgeStateBuildTraceRefineGoalsTheory`.

  Why this is useful
  - It makes the eventual translator proof plan explicit as a checklist:
      - extract a non-zero bucket count,
      - justify the trace,
      - justify the list observation,
      - justify the contains observation,
    then recombine.

  Status
  - The component bridges are currently `cheat`ed, but the recombination step
    is a real proof using the OK lemma
    `paramhash_build_trace_components_imp_build_state_ok`.
*)

open HolKernel Parse boolLib bossLib;

open F4FPPVerifyFastComputeBridgeGoalsTheory;
open F4FPPVerifyParamHashBridgeStateBuildTraceGoalsTheory;
open F4FPPVerifyParamHashBridgeStateBuildTraceRefineGoalsTheory;

val _ = new_theory "F4FPPVerifyParamHashBridgeStateBuildTraceRefineCheatsGoals";

Theorem fast_compute_program_succeeds_imp_paramhash_build_m_ok:
  !g. fast_compute_program_succeeds g ==> paramhash_build_m_ok g
Proof
  cheat
QED

Theorem fast_compute_program_succeeds_imp_paramhash_build_ps_ok:
  !g. fast_compute_program_succeeds g ==> paramhash_build_ps_ok g
Proof
  cheat
QED

Theorem fast_compute_program_succeeds_imp_paramhash_build_list_observes:
  !g. fast_compute_program_succeeds g ==> paramhash_build_list_observes g
Proof
  cheat
QED

Theorem fast_compute_program_succeeds_imp_paramhash_build_contains_observes:
  !g. fast_compute_program_succeeds g ==> paramhash_build_contains_observes g
Proof
  cheat
QED

Theorem fast_compute_program_succeeds_imp_paramhash_build_state_ok_decomposed:
  !g. fast_compute_program_succeeds g ==> paramhash_build_state_ok g
Proof
  rpt strip_tac
  \\ match_mp_tac paramhash_build_trace_components_imp_build_state_ok
  \\ metis_tac
      [ fast_compute_program_succeeds_imp_paramhash_build_m_ok
      , fast_compute_program_succeeds_imp_paramhash_build_ps_ok
      , fast_compute_program_succeeds_imp_paramhash_build_list_observes
      , fast_compute_program_succeeds_imp_paramhash_build_contains_observes
      ]
QED

val _ = export_theory ();

