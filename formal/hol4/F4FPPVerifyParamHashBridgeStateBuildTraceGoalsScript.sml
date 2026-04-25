(*
  File: formal/hol4/F4FPPVerifyParamHashBridgeStateBuildTraceGoalsScript.sml

  Purpose
  - Refine the existential build-witness predicate `paramhash_build_witness g`
    into a more translator-friendly *named model*:
      - a bucket-count `paramhash_build_m g`, and
      - a trace of attempted insert/match calls `paramhash_build_ps g`,
    together defining a canonical pure reference state:
      `paramhash_build_state g = ph_build_from_create_state (paramhash_build_m g)
                                                (paramhash_build_ps g)`.

  Why this exists
  - Translator/CakeML proofs typically produce an explicit *trace* and *state*
    rather than an existential witness.
  - By naming the model, we can state bridge theorems of the shape:
        `fast_compute_program_succeeds g ==> paramhash_build_state_ok g`
    and then derive `paramhash_build_witness g` and `paramhash_state_ok g` via
    OK lemmas.

  Status
  - This file is “OK”: it introduces only constants/definitions and proves
    implication lemmas with no `cheat`.
  - The concrete bridge from execution to these constants is recorded as
    `cheat` in a separate theory.
*)

open HolKernel Parse boolLib bossLib;

open F4FPPVerifyParamHashStateGoalsTheory;
open F4FPPVerifyParamHashStateBuildGoalsTheory;
open F4FPPVerifyParamHashBridgeStateBuildGoalsTheory;

val _ = new_theory "F4FPPVerifyParamHashBridgeStateBuildTraceGoals";

(* Abstract extracted build parameters:
   - `paramhash_build_m g` is the bucket-count used to create the table.
   - `paramhash_build_ps g` is the trace of attempted `match/insert` calls. *)
val _ = new_constant ("paramhash_build_m", ``:group -> num``);
val _ = new_constant ("paramhash_build_ps", ``:group -> param list``);

Definition paramhash_build_state_def:
  paramhash_build_state g =
    ph_build_from_create_state (paramhash_build_m g) (paramhash_build_ps g)
End

(* Canonical build-state correctness: the extracted parameters are well-formed,
   and the concrete observations agree with the canonical pure state. *)
Definition paramhash_build_state_ok_def:
  paramhash_build_state_ok g <=>
    paramhash_build_m g <> 0 /\
    paramhash_observes_state g (paramhash_build_state g)
End

Theorem paramhash_build_state_ok_imp_paramhash_build_witness:
  !g. paramhash_build_state_ok g ==> paramhash_build_witness g
Proof
  rw[paramhash_build_state_ok_def, paramhash_build_witness_def, paramhash_build_state_def]
  \\ qexists_tac `paramhash_build_m g`
  \\ qexists_tac `paramhash_build_ps g`
  \\ simp[]
QED

val _ = export_theory ();

