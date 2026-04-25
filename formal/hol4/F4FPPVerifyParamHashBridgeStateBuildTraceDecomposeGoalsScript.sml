(*
  File: formal/hol4/F4FPPVerifyParamHashBridgeStateBuildTraceDecomposeGoalsScript.sml

  Purpose
  - Provide an even more specified “build-state factored” ParamHash obligation
    bundle, using the canonical trace-based model:
        `paramhash_build_state_ok g`
    (from `F4FPPVerifyParamHashBridgeStateBuildTraceGoalsTheory`)
    instead of the existential `paramhash_build_witness g`.

  Rationale
  - A translator/CakeML proof typically produces explicit witnesses for:
      - the allocation parameter `m`, and
      - the operation trace `ps`,
    and then proves that the resulting pure model matches the observations.
  - This file names the corresponding bundle so later proofs can target it
    directly, and then discharge the older existential bundles via OK lemmas.

  Status
  - This file is “OK”: definitions and implication lemmas only.
*)

open HolKernel Parse boolLib bossLib;

open F4FPPVerifyAtlasFFIContractsGoalsTheory;
open F4FPPVerifyParamHashBridgeDecomposeGoalsTheory;
open F4FPPVerifyParamHashBridgeStateBuildGoalsTheory;
open F4FPPVerifyParamHashBridgeStateBuildDecomposeGoalsTheory;
open F4FPPVerifyParamHashBridgeStateBuildTraceGoalsTheory;

val _ = new_theory "F4FPPVerifyParamHashBridgeStateBuildTraceDecomposeGoals";

Definition paramhash_obligations_build_state_factored_def:
  paramhash_obligations_build_state_factored g <=>
    fast_param_set_is_paramhash g /\
    paramhash_build_state_ok g /\
    paramhash_stores_U_fast g
End

Definition paramhash_obligations_build_state_factored_atlas_eq_def:
  paramhash_obligations_build_state_factored_atlas_eq g <=>
    fast_param_set_is_paramhash g /\
    paramhash_build_state_ok g /\
    paramhash_stores_U_fast_atlas_eq g
End

Theorem paramhash_build_state_factored_imp_build_factored:
  !g.
    paramhash_obligations_build_state_factored g ==>
      paramhash_obligations_build_factored g
Proof
  rw[paramhash_obligations_build_state_factored_def,
     paramhash_obligations_build_factored_def]
  \\ metis_tac[paramhash_build_state_ok_imp_paramhash_build_witness]
QED

Theorem paramhash_build_state_factored_atlas_eq_imp_build_factored_atlas_eq:
  !g.
    paramhash_obligations_build_state_factored_atlas_eq g ==>
      paramhash_obligations_build_factored_atlas_eq g
Proof
  rw[paramhash_obligations_build_state_factored_atlas_eq_def,
     paramhash_obligations_build_factored_atlas_eq_def]
  \\ metis_tac[paramhash_build_state_ok_imp_paramhash_build_witness]
QED

(* Under the minimal hash-range contract, the build-state bundle implies the
   earlier state-factored bundle. *)
Theorem atlas_hash_range_and_build_state_factored_imp_state_factored:
  !g.
    atlas_hash_range /\ paramhash_obligations_build_state_factored g ==>
      paramhash_obligations_state_factored g
Proof
  rpt strip_tac
  \\ match_mp_tac atlas_hash_range_and_build_factored_imp_state_factored
  \\ metis_tac[paramhash_build_state_factored_imp_build_factored]
QED

Theorem atlas_hash_range_and_build_state_factored_atlas_eq_imp_state_factored_atlas_eq:
  !g.
    atlas_hash_range /\ paramhash_obligations_build_state_factored_atlas_eq g ==>
      paramhash_obligations_state_factored_atlas_eq g
Proof
  rpt strip_tac
  \\ match_mp_tac atlas_hash_range_and_build_factored_atlas_eq_imp_state_factored_atlas_eq
  \\ metis_tac[paramhash_build_state_factored_atlas_eq_imp_build_factored_atlas_eq]
QED

val _ = export_theory ();

