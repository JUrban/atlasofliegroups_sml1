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
  - OK (no `cheat`): definitions only.
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

val _ = export_theory ();
