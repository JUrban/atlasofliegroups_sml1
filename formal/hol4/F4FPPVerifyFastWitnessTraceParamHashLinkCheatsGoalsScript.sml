(*
  File: formal/hol4/F4FPPVerifyFastWitnessTraceParamHashLinkCheatsGoalsScript.sml

  Purpose
  - Record bridge placeholders connecting fast compute-phase success to the
    “trace linking” obligations between:
      - the witness/insert-event trace `fast_insert_trace g`, and
      - the ParamHash build trace `paramhash_build_ps g`.

  Status
  - Bridge theorems are currently `cheat`ed placeholders.
*)

open HolKernel Parse boolLib bossLib;

open F4FPPVerifyFastComputeBridgeGoalsTheory;
open F4FPPVerifyFastWitnessTraceParamHashLinkGoalsTheory;

val _ = new_theory "F4FPPVerifyFastWitnessTraceParamHashLinkCheatsGoals";

Theorem fast_compute_program_succeeds_imp_fast_insert_trace_params_ok:
  !g.
    fast_compute_program_succeeds g ==>
      fast_insert_trace_params_ok g
Proof
  (*
    Intended proof (later, without `cheat`):
    - show that the ParamHash trace witness `paramhash_build_ps g` is exactly
      the list of params appearing in the insertion/match event trace
      `fast_insert_trace g` (either in the same order, or after appropriate
      normalisation of “events”).
  *)
  cheat
QED

Theorem fast_compute_program_succeeds_imp_fast_insert_trace_params_sound_U_fast_atlas_eq:
  !g.
    fast_compute_program_succeeds g ==>
      fast_insert_trace_params_sound_U_fast_atlas_eq g
Proof
  (*
    Intended proof (later, without `cheat`):
    - show that every param appearing in the insert-event trace is represented
      (up to `atlas_eq`) in the abstract fast set `U_fast g`.
  *)
  cheat
QED

val _ = export_theory ();

