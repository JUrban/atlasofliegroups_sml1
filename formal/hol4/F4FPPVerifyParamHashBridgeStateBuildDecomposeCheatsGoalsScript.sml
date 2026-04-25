(*
  File: formal/hol4/F4FPPVerifyParamHashBridgeStateBuildDecomposeCheatsGoalsScript.sml

  Purpose
  - Isolate the (currently `cheat`ed) bridge theorems that connect fast compute
    phase success (`fast_compute_program_succeeds`) to the *build-factored*
    ParamHash obligations introduced in
    `F4FPPVerifyParamHashBridgeStateBuildDecomposeGoalsTheory`.

  Rationale / intended split
  - The eventual proof boundary is expected to be:
      (A) a translator/CakeML proof of the ParamHash data-structure core,
          yielding `paramhash_build_witness g`, and
      (B) an algorithmic argument about the fast compute phase inserting exactly
          `U_fast g`, yielding `paramhash_stores_U_fast(_atlas_eq) g`, and
      (C) “wiring” that connects the `param_set` view used by the bottom-layer
          checker to the ParamHash observations (`fast_param_set_is_paramhash`).

  This file records those as separate cheated sub-lemmas, then packages them
  into the build-factored bundles.
*)

open HolKernel Parse boolLib bossLib;

open F4FPPVerifyFastComputeBridgeGoalsTheory;
open F4FPPVerifyParamHashBridgeStateDecomposeCheatsGoalsTheory;
open F4FPPVerifyParamHashBridgeStateBuildCheatsGoalsTheory;
open F4FPPVerifyParamHashBridgeStateBuildDecomposeGoalsTheory;

val _ = new_theory "F4FPPVerifyParamHashBridgeStateBuildDecomposeCheatsGoals";

Theorem fast_compute_program_succeeds_imp_paramhash_obligations_build_factored:
  !g. fast_compute_program_succeeds g ==> paramhash_obligations_build_factored g
Proof
  rpt strip_tac
  \\ rw[paramhash_obligations_build_factored_def]
  \\ metis_tac
      [ fast_compute_program_succeeds_imp_fast_param_set_is_paramhash
      , fast_compute_program_succeeds_imp_paramhash_build_witness
      , fast_compute_program_succeeds_imp_paramhash_stores_U_fast
      ]
QED

Theorem fast_compute_program_succeeds_imp_paramhash_obligations_build_factored_atlas_eq:
  !g. fast_compute_program_succeeds g ==> paramhash_obligations_build_factored_atlas_eq g
Proof
  rpt strip_tac
  \\ rw[paramhash_obligations_build_factored_atlas_eq_def]
  \\ metis_tac
      [ fast_compute_program_succeeds_imp_fast_param_set_is_paramhash
      , fast_compute_program_succeeds_imp_paramhash_build_witness
      , fast_compute_program_succeeds_imp_paramhash_stores_U_fast_atlas_eq
      ]
QED

val _ = export_theory ();

