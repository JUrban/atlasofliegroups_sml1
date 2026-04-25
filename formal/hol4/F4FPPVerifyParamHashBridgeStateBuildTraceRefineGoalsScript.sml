(*
  File: formal/hol4/F4FPPVerifyParamHashBridgeStateBuildTraceRefineGoalsScript.sml

  Purpose
  - Provide a more detailed, translator-friendly decomposition of the canonical
    trace-based ParamHash build-model goal `paramhash_build_state_ok g`.

  Motivation
  - `paramhash_build_state_ok g` is a compact statement:
        `m <> 0 /\ paramhash_observes_state g (ph_build_from_create_state m ps)`
    where `m` and `ps` are abstractly extracted from execution.
  - For CakeML/translator proofs it is often easier to prove smaller facts and
    then recombine them:
      (1) the extracted bucket count is non-zero,
      (2) the extracted trace `ps` is the trace actually executed (optional),
      (3) the concrete `list` observation matches the pure model’s `elems`,
      (4) the concrete `contains` observation matches `ph_contains_state`.

  Scope
  - This file is “OK”: it introduces definitions and recombination lemmas only.
  - The bridge from `fast_compute_program_succeeds` to these sub-obligations is
    recorded (as `cheat`) in a separate `*Cheats*` theory.
*)

open HolKernel Parse boolLib bossLib;

open F4FPPVerifyParamHashStateGoalsTheory;
open F4FPPVerifyParamHashBridgeGoalsTheory;
open F4FPPVerifyParamHashBridgeStateBuildTraceGoalsTheory;

val _ = new_theory "F4FPPVerifyParamHashBridgeStateBuildTraceRefineGoals";

(* (1) Allocation parameter is well-formed. *)
Definition paramhash_build_m_ok_def:
  paramhash_build_m_ok g <=>
    paramhash_build_m g <> 0
End

(* (2) Optional trace well-formedness hook.
   For now this is deliberately weak: later we can strengthen it to express
   “`paramhash_build_ps g` is exactly the trace of attempted match/insert calls
   executed by the compute phase”. *)
Definition paramhash_build_ps_ok_def:
  paramhash_build_ps_ok g <=>
    T
End

(* (3) List observation matches the pure build model. *)
Definition paramhash_build_list_observes_def:
  paramhash_build_list_observes g <=>
    paramhash_list g = (paramhash_build_state g).elems
End

(* (4) Contains observation matches the pure build model. *)
Definition paramhash_build_contains_observes_def:
  paramhash_build_contains_observes g <=>
    !p. paramhash_contains g p <=> ph_contains_state p (paramhash_build_state g)
End

(* Recombine list+contains observations into the existing `paramhash_observes_state`. *)
Theorem paramhash_build_list_and_contains_observes_imp_observes_state:
  !g.
    paramhash_build_list_observes g /\ paramhash_build_contains_observes g ==>
      paramhash_observes_state g (paramhash_build_state g)
Proof
  rw[paramhash_build_list_observes_def, paramhash_build_contains_observes_def,
     paramhash_observes_state_def]
QED

Theorem paramhash_build_trace_components_imp_build_state_ok:
  !g.
    paramhash_build_m_ok g /\
    paramhash_build_ps_ok g /\
    paramhash_build_list_observes g /\
    paramhash_build_contains_observes g ==>
      paramhash_build_state_ok g
Proof
  rw[paramhash_build_state_ok_def, paramhash_build_m_ok_def]
  \\ metis_tac[paramhash_build_list_and_contains_observes_imp_observes_state]
QED

val _ = export_theory ();

