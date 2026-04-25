(*
  File: formal/hol4/F4FPPVerifyParamHashBridgeStateBuildTraceDecomposeCheatsGoalsScript.sml

  Purpose
  - Isolate the (currently `cheat`ed) bridge theorems connecting fast compute
    phase success to the *build-state factored* ParamHash obligations:
        `paramhash_obligations_build_state_factored(_atlas_eq)`.

  Intended split
  - Wiring: program constructs the `param_set` view directly from ParamHash.
  - Trace model: program execution yields `paramhash_build_state_ok g` for the
    canonical trace-based pure model.
  - Stores-U-fast: the compute phase inserts exactly the fast-set `U_fast g`.
*)

open HolKernel Parse boolLib bossLib;

open F4FPPVerifyFastComputeBridgeGoalsTheory;
open F4FPPVerifyParamHashBridgeStateDecomposeCheatsGoalsTheory;
open F4FPPVerifyParamHashBridgeStateBuildTraceCheatsGoalsTheory;
open F4FPPVerifyParamHashBridgeStateBuildTraceDecomposeGoalsTheory;

val _ = new_theory "F4FPPVerifyParamHashBridgeStateBuildTraceDecomposeCheatsGoals";

Theorem fast_compute_program_succeeds_imp_paramhash_obligations_build_state_factored:
  !g. fast_compute_program_succeeds g ==> paramhash_obligations_build_state_factored g
Proof
  rpt strip_tac
  \\ rw[paramhash_obligations_build_state_factored_def]
  \\ metis_tac
      [ fast_compute_program_succeeds_imp_fast_param_set_is_paramhash
      , fast_compute_program_succeeds_imp_paramhash_build_state_ok
      , fast_compute_program_succeeds_imp_paramhash_stores_U_fast
      ]
QED

Theorem fast_compute_program_succeeds_imp_paramhash_obligations_build_state_factored_atlas_eq:
  !g. fast_compute_program_succeeds g ==> paramhash_obligations_build_state_factored_atlas_eq g
Proof
  rpt strip_tac
  \\ rw[paramhash_obligations_build_state_factored_atlas_eq_def]
  \\ metis_tac
      [ fast_compute_program_succeeds_imp_fast_param_set_is_paramhash
      , fast_compute_program_succeeds_imp_paramhash_build_state_ok
      , fast_compute_program_succeeds_imp_paramhash_stores_U_fast_atlas_eq
      ]
QED

val _ = export_theory ();

