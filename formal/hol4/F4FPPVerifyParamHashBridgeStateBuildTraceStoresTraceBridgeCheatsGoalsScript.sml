(*
  File: formal/hol4/F4FPPVerifyParamHashBridgeStateBuildTraceStoresTraceBridgeCheatsGoalsScript.sml

  Purpose
  - Record the intended “translator target” bridge theorem for the new trace-level
    stores predicate `paramhash_build_ps_stores_U_fast_atlas_eq`:
        `fast_compute_program_succeeds g ==> paramhash_build_ps_stores_U_fast_atlas_eq g`.

  Motivation
  - This isolates a proof obligation that is closer to concrete computation:
    show that the list-valued trace witness `paramhash_build_ps g` represents the
    fast set `U_fast g` (modulo `atlas_eq`).
  - Together with the (CHEAT-tainted) upgrade lemmas from
    `F4FPPVerifyParamHashBridgeStateBuildTraceStoresTraceCheatsGoalsTheory`, this
    yields a fully stated route from compute success to extensional ParamHash
    obligations, without needing to mention `ph_set` at the bridge boundary.

  Status
  - The core bridge theorem is currently `cheat`ed; it is an explicit placeholder
    for future CakeML/translator work.
*)

open HolKernel Parse boolLib bossLib;

open F4FPPVerifyAtlasFFIContractsGoalsTheory;
open F4FPPVerifyFastComputeBridgeGoalsTheory;
open F4FPPVerifyParamHashBridgeDecomposeGoalsTheory;
open F4FPPVerifyParamHashBridgeStateDecomposeCheatsGoalsTheory;
open F4FPPVerifyParamHashBridgeStateBuildTraceCheatsGoalsTheory;
open F4FPPVerifyParamHashBridgeStateBuildTraceStoresTraceDecomposeGoalsTheory;
open F4FPPVerifyParamHashBridgeStateBuildTraceStoresTraceDecomposeCheatsGoalsTheory;
open F4FPPVerifyParamHashBridgeStateBuildTraceStoresTraceStoreDecomposeGoalsTheory;
open F4FPPVerifyParamHashBridgeStateBuildTraceStoresTraceToStateGoalsTheory;

val _ = new_theory "F4FPPVerifyParamHashBridgeStateBuildTraceStoresTraceBridgeCheatsGoals";

Theorem fast_compute_program_succeeds_imp_paramhash_build_ps_complete_U_fast_atlas_eq:
  !g.
    fast_compute_program_succeeds g ==>
      paramhash_build_ps_complete_U_fast_atlas_eq g
Proof
  (*
    Intended proof (later, without `cheat`):
    - show every element inserted into the abstract fast set `U_fast g`
      appears (up to `atlas_eq`) in the concrete insertion trace
      `paramhash_build_ps g`.
  *)
  cheat
QED

Theorem fast_compute_program_succeeds_imp_paramhash_build_ps_sound_U_fast_atlas_eq:
  !g.
    fast_compute_program_succeeds g ==>
      paramhash_build_ps_sound_U_fast_atlas_eq g
Proof
  (*
    Intended proof (later, without `cheat`):
    - show every element recorded in `paramhash_build_ps g` is indeed an
      element of the abstract fast set `U_fast g` (up to `atlas_eq`).
  *)
  cheat
QED

Theorem atlas_hash_eq_ok_and_fast_compute_program_succeeds_imp_paramhash_build_ps_stores_U_fast_atlas_eq:
  !g.
    atlas_hash_eq_ok /\ fast_compute_program_succeeds g ==>
      paramhash_build_ps_stores_U_fast_atlas_eq g
Proof
  rpt strip_tac
  \\ fs[atlas_hash_eq_ok_def]
  \\ match_mp_tac build_ps_sound_and_complete_imp_build_ps_stores_U_fast_atlas_eq
  \\ metis_tac
       [ fast_compute_program_succeeds_imp_paramhash_build_ps_complete_U_fast_atlas_eq
       , fast_compute_program_succeeds_imp_paramhash_build_ps_sound_U_fast_atlas_eq
       ]
QED

Theorem fast_compute_program_succeeds_imp_paramhash_build_ps_stores_U_fast_atlas_eq:
  !g.
    fast_compute_program_succeeds g ==>
      paramhash_build_ps_stores_U_fast_atlas_eq g
Proof
  (*
    Intended proof (later, without `cheat`):
    - show the compute phase’s insertion trace (captured abstractly as the list
      `paramhash_build_ps g`) corresponds exactly to the set `U_fast g`, modulo
      `atlas_eq`.

    This is the “semantic content” of the compute phase: it connects the
    program’s list-level trace witness to the abstract set-level spec.
  *)
  cheat
QED

Theorem fast_compute_program_succeeds_imp_paramhash_obligations_build_state_trace_stores_factored_atlas_eq:
  !g.
    fast_compute_program_succeeds g ==>
      paramhash_obligations_build_state_trace_stores_factored_atlas_eq g
Proof
  rpt strip_tac
  \\ rw[paramhash_obligations_build_state_trace_stores_factored_atlas_eq_def]
  \\ metis_tac
      [ fast_compute_program_succeeds_imp_fast_param_set_is_paramhash
      , fast_compute_program_succeeds_imp_paramhash_build_state_ok
      , fast_compute_program_succeeds_imp_paramhash_build_ps_stores_U_fast_atlas_eq
      ]
QED

Theorem atlas_hash_eq_ok_and_fast_compute_program_succeeds_imp_paramhash_obligations_build_state_trace_stores_factored_atlas_eq:
  !g.
    atlas_hash_eq_ok /\ fast_compute_program_succeeds g ==>
      paramhash_obligations_build_state_trace_stores_factored_atlas_eq g
Proof
  rpt strip_tac
  \\ rw[paramhash_obligations_build_state_trace_stores_factored_atlas_eq_def]
  \\ metis_tac
      [ fast_compute_program_succeeds_imp_fast_param_set_is_paramhash
      , fast_compute_program_succeeds_imp_paramhash_build_state_ok
      , atlas_hash_eq_ok_and_fast_compute_program_succeeds_imp_paramhash_build_ps_stores_U_fast_atlas_eq
      ]
QED

Theorem atlas_hash_eq_ok_and_fast_compute_program_succeeds_imp_paramhash_obligations_factored_atlas_eq_via_build_state_trace_stores:
  !g.
    atlas_hash_eq_ok /\ fast_compute_program_succeeds g ==>
      paramhash_obligations_factored_atlas_eq g
Proof
  rpt strip_tac
  \\ match_mp_tac atlas_hash_eq_ok_and_build_state_trace_stores_factored_atlas_eq_imp_paramhash_obligations_factored_atlas_eq
  \\ metis_tac[atlas_hash_eq_ok_and_fast_compute_program_succeeds_imp_paramhash_obligations_build_state_trace_stores_factored_atlas_eq]
QED

Theorem atlas_hash_eq_ok_and_fast_compute_program_succeeds_imp_paramhash_obligations_state_factored_atlas_eq_via_build_state_trace_stores:
  !g.
    atlas_hash_eq_ok /\ fast_compute_program_succeeds g ==>
      paramhash_obligations_state_factored_atlas_eq g
Proof
  rpt strip_tac
  \\ match_mp_tac atlas_hash_eq_ok_and_build_state_trace_stores_factored_atlas_eq_imp_paramhash_obligations_state_factored_atlas_eq
  \\ metis_tac[atlas_hash_eq_ok_and_fast_compute_program_succeeds_imp_paramhash_obligations_build_state_trace_stores_factored_atlas_eq]
QED

val _ = export_theory ();
