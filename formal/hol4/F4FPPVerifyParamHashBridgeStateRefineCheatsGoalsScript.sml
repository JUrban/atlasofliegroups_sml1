(*
  File: formal/hol4/F4FPPVerifyParamHashBridgeStateRefineCheatsGoalsScript.sml

  Purpose
  - Isolate the (currently `cheat`ed) bridge lemmas that connect fast compute
    phase success (`fast_compute_program_succeeds`) to the *state-level*
    ParamHash obligations introduced in
    `F4FPPVerifyParamHashBridgeStateRefineGoalsTheory`.

  Rationale
  - The corresponding `*Goals*` theory contains only definitions and pure
    refinement lemmas (no `cheat`).
  - The statements here are the intended attachment point for future CakeML
    proofs about imperative state + invariants, plus Atlas/FFI contracts.
*)

open HolKernel Parse boolLib bossLib;

open F4FPPVerifyAtlasFFIContractsGoalsTheory;
open F4FPPVerifyFastComputeBridgeGoalsTheory;
open F4FPPVerifyParamHashBridgeStateRefineGoalsTheory;

val _ = new_theory "F4FPPVerifyParamHashBridgeStateRefineCheatsGoals";

Theorem fast_compute_program_succeeds_imp_paramhash_observation_witness:
  !g. fast_compute_program_succeeds g ==> paramhash_observation_witness g
Proof
  (*
    Intended proof (later, without `cheat`):
    - connect the concrete ParamHash heap object to an abstract state `s`
      satisfying `paramhash_observes_state`.
  *)
  cheat
QED

Theorem fast_compute_program_succeeds_imp_paramhash_ok_on_observation:
  !g. fast_compute_program_succeeds g ==> paramhash_ok_on_observation g
Proof
  (*
    Intended proof (later, without `cheat`):
    - show any observed abstract state satisfies `ph_ok`:
        bucket counts, bucket lengths, and index correctness.
  *)
  cheat
QED

Theorem fast_compute_program_succeeds_imp_paramhash_bucketed_on_observation:
  !g. fast_compute_program_succeeds g ==> paramhash_bucketed_on_observation g
Proof
  (*
    Intended proof (later, without `cheat`):
    - show any observed abstract state satisfies `ph_bucketed`:
        every bucket entry is stored in the bucket determined by `atlas_hash_mod`.
  *)
  cheat
QED

Theorem fast_compute_program_succeeds_imp_paramhash_covered_on_observation:
  !g. fast_compute_program_succeeds g ==> paramhash_covered_on_observation g
Proof
  (*
    Intended proof (later, without `cheat`):
    - show any observed abstract state satisfies `ph_covered`:
        every element in the enumerated list appears at its index in the buckets.
  *)
  cheat
QED

Theorem fast_compute_program_succeeds_imp_paramhash_invariant_on_observation:
  !g. fast_compute_program_succeeds g ==> paramhash_invariant_on_observation g
Proof
  rpt strip_tac
  \\ match_mp_tac paramhash_ok_bucketed_covered_imp_invariant_on_observation
  \\ metis_tac
      [ fast_compute_program_succeeds_imp_paramhash_ok_on_observation
      , fast_compute_program_succeeds_imp_paramhash_bucketed_on_observation
      , fast_compute_program_succeeds_imp_paramhash_covered_on_observation
      ]
QED

Theorem fast_compute_program_succeeds_imp_paramhash_state_ok_decomposed:
  !g. fast_compute_program_succeeds g ==> paramhash_state_ok g
Proof
  rpt strip_tac
  \\ match_mp_tac paramhash_observation_witness_and_invariant_imp_state_ok
  \\ metis_tac
      [ fast_compute_program_succeeds_imp_paramhash_observation_witness
      , fast_compute_program_succeeds_imp_paramhash_invariant_on_observation
      ]
QED

Theorem fast_compute_program_succeeds_imp_paramhash_state_ok:
  !g. fast_compute_program_succeeds g ==> paramhash_state_ok g
Proof
  metis_tac[fast_compute_program_succeeds_imp_paramhash_state_ok_decomposed]
QED

Theorem fast_compute_program_succeeds_imp_paramhash_rep_ok_via_state:
  !g.
    atlas_eq_is_hol_eq /\ atlas_hash_range /\ fast_compute_program_succeeds g ==>
      paramhash_rep_ok g
Proof
  rpt strip_tac
  \\ match_mp_tac paramhash_state_ok_imp_paramhash_rep_ok
  \\ metis_tac[fast_compute_program_succeeds_imp_paramhash_state_ok]
QED

Theorem fast_compute_program_succeeds_imp_paramhash_rep_ok_atlas_eq_via_state:
  !g.
    atlas_hash_eq_ok /\ fast_compute_program_succeeds g ==>
      paramhash_rep_ok_atlas_eq g
Proof
  rpt strip_tac
  \\ match_mp_tac paramhash_state_ok_imp_paramhash_rep_ok_atlas_eq
  \\ metis_tac[fast_compute_program_succeeds_imp_paramhash_state_ok]
QED

val _ = export_theory ();
