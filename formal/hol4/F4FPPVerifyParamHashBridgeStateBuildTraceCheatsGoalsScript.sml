(*
  File: formal/hol4/F4FPPVerifyParamHashBridgeStateBuildTraceCheatsGoalsScript.sml

  Purpose
  - Isolate the (currently `cheat`ed) bridge theorem that connects fast compute
    phase success to the *trace-based* canonical build model introduced in
    `F4FPPVerifyParamHashBridgeStateBuildTraceGoalsTheory`.

  Intended future proof (CakeML/translator)
  - Produce concrete witnesses:
      - `paramhash_build_m g` from the ParamHash allocation code, and
      - `paramhash_build_ps g` as a trace of calls to the table’s `match`/insert
        operation during the compute phase.
  - Prove:
      - the canonical pure state `paramhash_build_state g` is observationally
        equivalent to the concrete table produced by the program:
          `paramhash_observes_state g (paramhash_build_state g)`.
*)

open HolKernel Parse boolLib bossLib;

open F4FPPVerifyFastComputeBridgeGoalsTheory;
open F4FPPVerifyParamHashBridgeStateBuildTraceGoalsTheory;

val _ = new_theory "F4FPPVerifyParamHashBridgeStateBuildTraceCheatsGoals";

Theorem fast_compute_program_succeeds_imp_paramhash_build_state_ok:
  !g. fast_compute_program_succeeds g ==> paramhash_build_state_ok g
Proof
  (*
    Intended proof (later, without `cheat`):
    - extract `paramhash_build_m g` and `paramhash_build_ps g` from the program’s
      execution and show the resulting pure model matches the observations.
  *)
  cheat
QED

val _ = export_theory ();

