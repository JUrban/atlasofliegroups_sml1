(*
  File: formal/hol4/F4FPPVerifyParamHashBridgeStateBuildTraceConsequencesGoalsScript.sml

  Purpose
  - Record useful *OK consequences* of the canonical trace-based build-model
    predicate `paramhash_build_state_ok g`.

  Why this matters
  - `paramhash_build_state_ok g` is intended to be the direct target of a future
    translator/CakeML proof: it names concrete witnesses (`m`, `ps`) and asserts
    the ParamHash observations match the pure build-state model.
  - Once we have that, we can derive (without any further cheating) key
    data-structure properties used elsewhere in the HOL4 stack, notably:
      - representation correctness of `contains` w.r.t. list membership modulo
        `atlas_eq` (`paramhash_rep_ok_atlas_eq g`).

  Status
  - This file is “OK”: it contains only logical consequences of previously
    proved invariant/representation theorems.
*)

open HolKernel Parse boolLib bossLib;

open F4FPPVerifyAtlasFFIContractsGoalsTheory;
open F4FPPVerifyAtlasEqListGoalsTheory;
open F4FPPVerifyParamHashBridgeGoalsTheory;
open F4FPPVerifyParamHashBridgeDecomposeGoalsTheory;
open F4FPPVerifyParamHashStateGoalsTheory;
open F4FPPVerifyParamHashStateBuildGoalsTheory;
open F4FPPVerifyParamHashBridgeStateBuildTraceGoalsTheory;

val _ = new_theory "F4FPPVerifyParamHashBridgeStateBuildTraceConsequencesGoals";

Theorem atlas_hash_eq_ok_and_paramhash_build_state_ok_imp_paramhash_rep_ok_atlas_eq:
  !g.
    atlas_hash_eq_ok /\ paramhash_build_state_ok g ==> paramhash_rep_ok_atlas_eq g
Proof
  rpt gen_tac
  \\ strip_tac
  \\ simp[paramhash_rep_ok_atlas_eq_def]
  \\ gen_tac
  \\ qpat_x_assum `paramhash_build_state_ok g` mp_tac
  \\ simp[paramhash_build_state_ok_def]
  \\ strip_tac
  \\ fs[paramhash_observes_state_def]
  \\ simp[]
  \\ (* `ph_contains_state p (paramhash_build_state g) <=> mem_atlas_eq p (paramhash_list g)` *)
     `ph_invariant (paramhash_build_state g)` by
       (fs[paramhash_build_state_def]
        \\ match_mp_tac ph_build_from_create_state_invariant
        \\ fs[atlas_hash_eq_ok_def])
  \\ metis_tac[ph_contains_state_iff_mem_atlas_eq_elems]
QED

val _ = export_theory ();
