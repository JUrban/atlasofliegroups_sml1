(*
  File: formal/hol4/F4FPPVerifyParamHashBridgeStateBuildDecomposeGoalsScript.sml

  Purpose
  - Add an intermediate *build-witness–factored* bundle for the ParamHash side
    of the fast-compute bridge.

  Background
  - The existing “state-factored” bundle
        `paramhash_obligations_state_factored g`
    uses the predicate `paramhash_state_ok g` (∃ invariant state observing the
    concrete `list/contains` view).
  - The newer “build witness” goal
        `paramhash_build_witness g`
    is more specified: it requires that the observations are explained by the
    *pure build model* `ph_build_from_create_state m ps`.

  Why introduce this bundle?
  - It makes the eventual translator proof boundary cleaner:
      (1) show a concrete trace/witness for a *pure build* state,
      (2) separately argue the compute phase inserted exactly `U_fast g`.
  - Once `paramhash_build_witness g` holds, the invariant part is discharged
    by the fully proved HOL4 theory `F4FPPVerifyParamHashStateBuildGoalsTheory`.

  Status
  - This file is “OK”: definitions and logical implications only.
  - Bridge theorems from execution are recorded separately in a `*Cheats*`
    theory.
*)

open HolKernel Parse boolLib bossLib;

open F4FPPVerifyAtlasFFIContractsGoalsTheory;
open F4FPPVerifyParamHashBridgeGoalsTheory;
open F4FPPVerifyParamHashBridgeDecomposeGoalsTheory;
open F4FPPVerifyParamHashBridgeStateBuildGoalsTheory;
open F4FPPVerifyParamHashBridgeStateDecomposeGoalsTheory;

val _ = new_theory "F4FPPVerifyParamHashBridgeStateBuildDecomposeGoals";

(* A “more specified” version of the state-factored bundle: instead of assuming
   `paramhash_state_ok`, we assume the concrete observations correspond to a
   pure build-state witness. *)
Definition paramhash_obligations_build_factored_def:
  paramhash_obligations_build_factored g <=>
    fast_param_set_is_paramhash g /\
    paramhash_build_witness g /\
    paramhash_stores_U_fast g
End

Definition paramhash_obligations_build_factored_atlas_eq_def:
  paramhash_obligations_build_factored_atlas_eq g <=>
    fast_param_set_is_paramhash g /\
    paramhash_build_witness g /\
    paramhash_stores_U_fast_atlas_eq g
End

Theorem atlas_hash_range_and_build_factored_imp_state_factored:
  !g.
    atlas_hash_range /\ paramhash_obligations_build_factored g ==>
      paramhash_obligations_state_factored g
Proof
  rw[paramhash_obligations_build_factored_def, paramhash_obligations_state_factored_def]
  \\ match_mp_tac atlas_hash_range_and_paramhash_build_witness_imp_paramhash_state_ok
  \\ metis_tac[]
QED

Theorem atlas_hash_range_and_build_factored_atlas_eq_imp_state_factored_atlas_eq:
  !g.
    atlas_hash_range /\ paramhash_obligations_build_factored_atlas_eq g ==>
      paramhash_obligations_state_factored_atlas_eq g
Proof
  rw[paramhash_obligations_build_factored_atlas_eq_def, paramhash_obligations_state_factored_atlas_eq_def]
  \\ match_mp_tac atlas_hash_range_and_paramhash_build_witness_imp_paramhash_state_ok
  \\ metis_tac[]
QED

(* Therefore, under the usual contracts, build-factored obligations imply the
   extensional factored obligations from `F4FPPVerifyParamHashBridgeDecomposeGoalsTheory`. *)
Theorem atlas_hash_range_and_build_factored_imp_paramhash_obligations_factored:
  !g.
    atlas_eq_is_hol_eq /\ atlas_hash_range /\ paramhash_obligations_build_factored g ==>
      paramhash_obligations_factored g
Proof
  rpt strip_tac
  \\ match_mp_tac paramhash_state_factored_imp_paramhash_obligations_factored
  \\ simp[]
  \\ match_mp_tac atlas_hash_range_and_build_factored_imp_state_factored
  \\ metis_tac[]
QED

Theorem atlas_hash_eq_ok_and_build_factored_atlas_eq_imp_paramhash_obligations_factored_atlas_eq:
  !g.
    atlas_hash_eq_ok /\ paramhash_obligations_build_factored_atlas_eq g ==>
      paramhash_obligations_factored_atlas_eq g
Proof
  rpt strip_tac
  \\ match_mp_tac paramhash_state_factored_atlas_eq_imp_paramhash_obligations_factored_atlas_eq
  \\ fs[atlas_hash_eq_ok_def]
  \\ match_mp_tac atlas_hash_range_and_build_factored_atlas_eq_imp_state_factored_atlas_eq
  \\ metis_tac[]
QED

val _ = export_theory ();

