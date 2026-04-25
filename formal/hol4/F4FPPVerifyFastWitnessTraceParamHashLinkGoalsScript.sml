(*
  File: formal/hol4/F4FPPVerifyFastWitnessTraceParamHashLinkGoalsScript.sml

  Purpose
  - Connect the fast “insert-event” witness trace
      `fast_insert_trace g : (triple # param) list`
    to the ParamHash build trace
      `paramhash_build_ps g : param list`.

  Motivation
  - The ParamHash trace route isolates obligations about the list witness
    `paramhash_build_ps g` (e.g. it represents `U_fast g` modulo `atlas_eq`).
  - Separately, the fast-side semantic route benefits from reasoning about a
    richer event trace that also records witness triples.
  - This theory provides OK lemmas that let a proof establish the ParamHash
    trace obligations by proving properties about `fast_insert_trace` plus a
    single “projection” agreement between the traces.

  Status
  - Definitions are OK.
  - The key logical bridge lemmas are currently `cheat`ed placeholders (they
    should be discharged by routine list/set reasoning once the exact list/set
    view of `MEM`/`set` is fixed in this development).
*)

open HolKernel Parse boolLib bossLib;

open listTheory listLib;
open pred_setTheory pred_setLib;

open F4FPPVerifyAtlasFFIContractsGoalsTheory;
open F4FPPVerifyAtlasEqListGoalsTheory;
open F4FPPVerifyAtlasEqSetGoalsTheory;

open F4FPPVerifyGoalsTheory;
open F4FPPVerifyFastWitnessTraceGoalsTheory;
open F4FPPVerifyParamHashBridgeStateBuildTraceGoalsTheory;
open F4FPPVerifyParamHashBridgeStateBuildTraceStoresTraceStoreDecomposeGoalsTheory;

val _ = new_theory "F4FPPVerifyFastWitnessTraceParamHashLinkGoals";

(* The ParamHash build trace is exactly the param-projection of the insert-event trace. *)
Definition fast_insert_trace_params_ok_def:
  fast_insert_trace_params_ok g <=>
    MAP SND (fast_insert_trace g) = paramhash_build_ps g
End

(* Every traced param is represented (modulo `atlas_eq`) in `U_fast`. *)
Definition fast_insert_trace_params_sound_U_fast_atlas_eq_def:
  fast_insert_trace_params_sound_U_fast_atlas_eq g <=>
    !t pi. MEM (t,pi) (fast_insert_trace g) ==> mem_set_atlas_eq pi (U_fast g)
End

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
