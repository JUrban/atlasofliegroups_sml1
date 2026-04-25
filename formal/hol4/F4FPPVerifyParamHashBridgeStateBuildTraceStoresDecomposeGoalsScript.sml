(*
  File: formal/hol4/F4FPPVerifyParamHashBridgeStateBuildTraceStoresDecomposeGoalsScript.sml

  Purpose
  - Add a ParamHash obligation bundle that is maximally aligned with what a
    translator/CakeML proof is expected to deliver from the compute phase:
      - a canonical trace-based build model (`paramhash_build_state_ok g`), and
      - a semantic “stores `U_fast`” obligation stated against that model state
        (`paramhash_build_stores_U_fast_atlas_eq g`).

  Compared to existing bundles
  - `paramhash_obligations_build_state_factored_atlas_eq` uses the *list-based*
    stores predicate `paramhash_stores_U_fast_atlas_eq`.
  - Here we instead use the *state-based* predicate from
    `F4FPPVerifyParamHashBridgeStateBuildTraceStoresGoalsTheory`.

  Status
  - This file is “OK”: definitions and implication lemmas only.
*)

open HolKernel Parse boolLib bossLib;

open F4FPPVerifyAtlasFFIContractsGoalsTheory;
open F4FPPVerifyParamHashBridgeDecomposeGoalsTheory;
open F4FPPVerifyParamHashBridgeStateBuildTraceGoalsTheory;
open F4FPPVerifyParamHashBridgeStateBuildTraceDecomposeGoalsTheory;
open F4FPPVerifyParamHashBridgeStateBuildTraceStoresGoalsTheory;
open F4FPPVerifyParamHashBridgeStateBuildTraceConsequencesGoalsTheory;

val _ = new_theory "F4FPPVerifyParamHashBridgeStateBuildTraceStoresDecomposeGoals";

Definition paramhash_obligations_build_state_stores_factored_atlas_eq_def:
  paramhash_obligations_build_state_stores_factored_atlas_eq g <=>
    fast_param_set_is_paramhash g /\
    paramhash_build_state_ok g /\
    paramhash_build_stores_U_fast_atlas_eq g
End

Theorem build_state_stores_factored_atlas_eq_imp_build_state_factored_atlas_eq:
  !g.
    paramhash_obligations_build_state_stores_factored_atlas_eq g ==>
      paramhash_obligations_build_state_factored_atlas_eq g
Proof
  rw[paramhash_obligations_build_state_stores_factored_atlas_eq_def,
     paramhash_obligations_build_state_factored_atlas_eq_def]
  \\ metis_tac[paramhash_build_state_ok_and_build_stores_imp_paramhash_stores_U_fast_atlas_eq]
QED

(* Under the usual hash/equality contracts, this strongest bundle implies the
   extensional `paramhash_obligations_factored_atlas_eq` obligations directly. *)
Theorem atlas_hash_eq_ok_and_build_state_stores_factored_atlas_eq_imp_paramhash_obligations_factored_atlas_eq:
  !g.
    atlas_hash_eq_ok /\ paramhash_obligations_build_state_stores_factored_atlas_eq g ==>
      paramhash_obligations_factored_atlas_eq g
Proof
  rpt strip_tac
  \\ fs[paramhash_obligations_build_state_stores_factored_atlas_eq_def,
        paramhash_obligations_factored_atlas_eq_def]
  \\ metis_tac
      [ atlas_hash_eq_ok_and_paramhash_build_state_ok_imp_paramhash_rep_ok_atlas_eq
      , paramhash_build_state_ok_and_build_stores_imp_paramhash_stores_U_fast_atlas_eq
      ]
QED

val _ = export_theory ();
