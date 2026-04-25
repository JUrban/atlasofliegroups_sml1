(*
  File: formal/hol4/F4FPPVerifyFastWitnessTraceParamHashLinkCheatsGoalsScript.sml

  Purpose
  - Record bridge placeholders connecting fast compute-phase success to the
    “trace linking” obligations between:
      - the witness/insert-event trace `fast_insert_trace g`, and
      - the ParamHash build trace `paramhash_build_ps g`.
  - Also record (currently `cheat`ed) logical consequence lemmas that turn the
    insert-trace obligations into the ParamHash build-trace sound/complete
    obligations.

  Status
  - Bridge theorems are currently `cheat`ed placeholders.
*)

open HolKernel Parse boolLib bossLib;

open F4FPPVerifyAtlasFFIContractsGoalsTheory;
open F4FPPVerifyFastComputeBridgeGoalsTheory;
open F4FPPVerifyFastWitnessTraceGoalsTheory;
open F4FPPVerifyFastWitnessTraceParamHashLinkGoalsTheory;
open F4FPPVerifyParamHashBridgeStateBuildTraceStoresTraceStoreDecomposeGoalsTheory;

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

Theorem atlas_eq_equiv_and_insert_trace_params_ok_and_covers_imp_build_ps_complete_U_fast_atlas_eq:
  !g.
    atlas_eq_equiv /\
    fast_insert_trace_params_ok g /\
    fast_insert_trace_covers_U_fast g ==>
      paramhash_build_ps_complete_U_fast_atlas_eq g
Proof
  (*
    TODO (remove `cheat`):
    - unfold `paramhash_build_ps_complete_U_fast_atlas_eq` and `mem_set_atlas_eq`;
    - use `fast_insert_trace_covers_U_fast` to obtain an event `(t,p)`;
    - use `fast_insert_trace_params_ok` to rewrite `paramhash_build_ps` as
      `MAP SND (fast_insert_trace g)` and show `p` is in that projection;
    - close with reflexivity from `atlas_eq_equiv`.
  *)
  cheat
QED

Theorem insert_trace_params_ok_and_params_sound_imp_build_ps_sound_U_fast_atlas_eq:
  !g.
    fast_insert_trace_params_ok g /\
    fast_insert_trace_params_sound_U_fast_atlas_eq g ==>
      paramhash_build_ps_sound_U_fast_atlas_eq g
Proof
  (*
    TODO (remove `cheat`):
    - unfold `paramhash_build_ps_sound_U_fast_atlas_eq`;
    - rewrite `p IN set (paramhash_build_ps g)` using `fast_insert_trace_params_ok`;
    - obtain an event `(t,p)` from membership in the projected set;
    - discharge with `fast_insert_trace_params_sound_U_fast_atlas_eq`.
  *)
  cheat
QED

val _ = export_theory ();
