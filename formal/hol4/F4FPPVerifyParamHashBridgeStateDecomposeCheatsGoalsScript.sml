(*
  File: formal/hol4/F4FPPVerifyParamHashBridgeStateDecomposeCheatsGoalsScript.sml

  Purpose
  - Isolate the (currently `cheat`ed) bridge theorems that connect fast compute
    phase success (`fast_compute_program_succeeds`) to the *state-level*
    bundled obligations:
      - `paramhash_obligations_state_factored`
      - `paramhash_obligations_state_factored_atlas_eq`

  Rationale
  - `F4FPPVerifyParamHashBridgeStateDecomposeGoalsTheory` is “OK” and contains
    only the definitions and logical implications from the state bundle to the
    extensional bundle.
  - The statements here are the intended target for CakeML/translator proofs
    about imperative state + invariants, plus Atlas/FFI contracts.
*)

open HolKernel Parse boolLib bossLib;

open F4FPPVerifyFastComputeBridgeGoalsTheory;
open F4FPPVerifyParamHashBridgeStateDecomposeGoalsTheory;

val _ = new_theory "F4FPPVerifyParamHashBridgeStateDecomposeCheatsGoals";

Theorem fast_compute_program_succeeds_imp_paramhash_obligations_state_factored:
  !g. fast_compute_program_succeeds g ==> paramhash_obligations_state_factored g
Proof
  (*
    Intended proof (later, without `cheat`):
    - wiring: `fast_param_set_is_paramhash g` follows from how the program
      constructs the `param_set` view of ParamHash,
    - state_ok: discharged by a CakeML/translator proof of `ph_invariant`,
    - stores_U_fast: algorithmic argument about what the compute phase inserts.
  *)
  cheat
QED

Theorem fast_compute_program_succeeds_imp_paramhash_obligations_state_factored_atlas_eq:
  !g. fast_compute_program_succeeds g ==> paramhash_obligations_state_factored_atlas_eq g
Proof
  (*
    Intended proof (later, without `cheat`):
    - wiring + state_ok are identical to the non-modulo bundle,
    - `paramhash_stores_U_fast_atlas_eq` is the “right” extensional statement
      for the stored list, modulo `atlas_eq`.
  *)
  cheat
QED

val _ = export_theory ();

