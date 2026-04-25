(*
  File: formal/hol4/F4FPPVerifyParamHashBridgeStateBuildTraceStoresDecomposeCheatsGoalsScript.sml

  Purpose
  - Provide a (cheat-tainted, but proof-by-composition) bridge from fast compute
    success to the strongest “build-state + stores” ParamHash bundle:
      `paramhash_obligations_build_state_stores_factored_atlas_eq`.

  Notes
  - This theorem introduces no new `cheat`s: it is derived from earlier cheated
    bridge lemmas plus OK implication lemmas.
  - The intent is to make the ultimate translator proof target explicit:
      (1) establish `paramhash_build_state_ok` for the canonical model, and
      (2) establish that this model stores `U_fast` (state-based, modulo `atlas_eq`).
*)

open HolKernel Parse boolLib bossLib;

open F4FPPVerifyFastComputeBridgeGoalsTheory;
open F4FPPVerifyParamHashBridgeStateDecomposeCheatsGoalsTheory;
open F4FPPVerifyParamHashBridgeStateBuildTraceCheatsGoalsTheory;
open F4FPPVerifyParamHashBridgeStateBuildTraceStoresGoalsTheory;
open F4FPPVerifyParamHashBridgeStateBuildTraceStoresDecomposeGoalsTheory;

val _ = new_theory "F4FPPVerifyParamHashBridgeStateBuildTraceStoresDecomposeCheatsGoals";

(* Derived bridge for the state-based stores predicate.

   This is an intended CakeML/translator target: show that the canonical
   build-state model stores exactly `U_fast` (modulo `atlas_eq`). *)
Theorem fast_compute_program_succeeds_imp_paramhash_build_stores_U_fast_atlas_eq:
  !g.
    fast_compute_program_succeeds g ==>
      paramhash_build_stores_U_fast_atlas_eq g
Proof
  rpt strip_tac
  \\ match_mp_tac paramhash_build_state_ok_and_paramhash_stores_U_fast_atlas_eq_imp_build_stores
  \\ conj_tac
  >- metis_tac[fast_compute_program_succeeds_imp_paramhash_build_state_ok]
  \\ metis_tac[fast_compute_program_succeeds_imp_paramhash_stores_U_fast_atlas_eq]
QED

Theorem fast_compute_program_succeeds_imp_paramhash_obligations_build_state_stores_factored_atlas_eq:
  !g.
    fast_compute_program_succeeds g ==>
      paramhash_obligations_build_state_stores_factored_atlas_eq g
Proof
  rpt strip_tac
  \\ simp[paramhash_obligations_build_state_stores_factored_atlas_eq_def]
  \\ conj_tac
  >- metis_tac[fast_compute_program_succeeds_imp_fast_param_set_is_paramhash]
  \\ conj_tac
  >- metis_tac[fast_compute_program_succeeds_imp_paramhash_build_state_ok]
  \\ metis_tac[fast_compute_program_succeeds_imp_paramhash_build_stores_U_fast_atlas_eq]
QED

val _ = export_theory ();
