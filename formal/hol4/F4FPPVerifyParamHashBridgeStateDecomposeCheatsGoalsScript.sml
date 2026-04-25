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
open F4FPPVerifyParamHashBridgeStateRefineCheatsGoalsTheory;

val _ = new_theory "F4FPPVerifyParamHashBridgeStateDecomposeCheatsGoals";

(* Smaller bridge obligations (still CHEATED), intended to be discharged by:
   - program-structure reasoning for the wiring,
   - CakeML/translator proofs for the state witness/invariant, and
   - an algorithmic argument for what the compute phase inserts. *)

Theorem fast_compute_program_succeeds_imp_fast_param_set_is_paramhash:
  !g. fast_compute_program_succeeds g ==> fast_param_set_is_paramhash g
Proof
  (*
    Intended proof (later, without `cheat`):
    - unfold the concrete construction of the param_set view passed to the
      bottom-layer checker; show its `ps_list`/`ps_contains` are exactly the
      ParamHash observations `paramhash_list`/`paramhash_contains`.
  *)
  cheat
QED

Theorem fast_compute_program_succeeds_imp_paramhash_stores_U_fast:
  !g. fast_compute_program_succeeds g ==> paramhash_stores_U_fast g
Proof
  (*
    Intended proof (later, without `cheat`):
    - show the compute phase inserts exactly the elements of `U_fast g` (as
      defined in the goal layer), hence the stored list represents `U_fast g`.
  *)
  cheat
QED

Theorem fast_compute_program_succeeds_imp_paramhash_stores_U_fast_atlas_eq:
  !g. fast_compute_program_succeeds g ==> paramhash_stores_U_fast_atlas_eq g
Proof
  (*
    Intended proof (later, without `cheat`):
    - as above, but phrased as extensional set equality modulo `atlas_eq`.
  *)
  cheat
QED

Theorem fast_compute_program_succeeds_imp_paramhash_obligations_state_factored:
  !g. fast_compute_program_succeeds g ==> paramhash_obligations_state_factored g
Proof
  rpt strip_tac
  \\ rw[paramhash_obligations_state_factored_def]
  \\ metis_tac
      [ fast_compute_program_succeeds_imp_fast_param_set_is_paramhash
      , fast_compute_program_succeeds_imp_paramhash_state_ok
      , fast_compute_program_succeeds_imp_paramhash_stores_U_fast
      ]
QED

Theorem fast_compute_program_succeeds_imp_paramhash_obligations_state_factored_atlas_eq:
  !g. fast_compute_program_succeeds g ==> paramhash_obligations_state_factored_atlas_eq g
Proof
  rpt strip_tac
  \\ rw[paramhash_obligations_state_factored_atlas_eq_def]
  \\ metis_tac
      [ fast_compute_program_succeeds_imp_fast_param_set_is_paramhash
      , fast_compute_program_succeeds_imp_paramhash_state_ok
      , fast_compute_program_succeeds_imp_paramhash_stores_U_fast_atlas_eq
      ]
QED

val _ = export_theory ();
