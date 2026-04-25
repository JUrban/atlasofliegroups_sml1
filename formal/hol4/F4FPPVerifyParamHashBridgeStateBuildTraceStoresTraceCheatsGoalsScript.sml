(*
  File: formal/hol4/F4FPPVerifyParamHashBridgeStateBuildTraceStoresTraceCheatsGoalsScript.sml

  Purpose
  - Provide a (currently cheat-tainted, but proof-by-composition) implication
    from a trace-level stores predicate to the state-based stores predicate for
    the canonical ParamHash build-state model.

  Why this matters
  - For translator/CakeML proofs it may be more convenient to establish that the
    collected insertion trace `paramhash_build_ps g` represents `U_fast g`
    (modulo `atlas_eq`) than to reason directly about `ph_set`.
  - This theory records the precise “upgrade lemma”:
        `U_fast ~ set(trace)`  ==>  `U_fast ~ ph_set(build_state)`,
    under the hash/equality contracts needed by the pure build model.

  Status
  - No new `cheat` is introduced in this file, but the result is CHEAT-tainted
    because it depends on the (currently CHEAT) build-set lemma in
    `F4FPPVerifyParamHashStateBuildSetGoalsTheory`.
*)

open HolKernel Parse boolLib bossLib;

open F4FPPVerifyAtlasFFIContractsGoalsTheory;
open F4FPPVerifyAtlasEqSetGoalsTheory;
open F4FPPVerifyParamHashBridgeStateBuildTraceGoalsTheory;
open F4FPPVerifyParamHashBridgeStateBuildTraceRefineGoalsTheory;
open F4FPPVerifyParamHashBridgeStateBuildTraceStoresGoalsTheory;
open F4FPPVerifyParamHashStateBuildSetGoalsTheory;

val _ = new_theory "F4FPPVerifyParamHashBridgeStateBuildTraceStoresTraceCheatsGoals";

Theorem atlas_hash_eq_ok_and_paramhash_build_m_ok_and_build_ps_stores_imp_build_stores:
  !g.
    atlas_hash_eq_ok /\
    paramhash_build_m_ok g /\
    paramhash_build_ps_stores_U_fast_atlas_eq g ==>
      paramhash_build_stores_U_fast_atlas_eq g
Proof
  rpt strip_tac
  \\ fs[atlas_hash_eq_ok_def]
  \\ fs[paramhash_build_m_ok_def]
  \\ fs[paramhash_build_ps_stores_U_fast_atlas_eq_def,
        paramhash_build_stores_U_fast_atlas_eq_def,
        paramhash_build_state_def]
  \\ `set_atlas_eq (set (paramhash_build_ps g))
        (ph_set (ph_build_from_create_state (paramhash_build_m g) (paramhash_build_ps g)))` by
       (match_mp_tac ph_build_from_create_state_set_atlas_eq_set_ps \\ simp[])
  \\ fs[set_atlas_eq_def]
  \\ metis_tac[]
QED

Theorem atlas_hash_eq_ok_and_paramhash_build_m_ok_and_build_stores_imp_build_ps_stores:
  !g.
    atlas_hash_eq_ok /\
    paramhash_build_m_ok g /\
    paramhash_build_stores_U_fast_atlas_eq g ==>
      paramhash_build_ps_stores_U_fast_atlas_eq g
Proof
  rpt strip_tac
  \\ fs[atlas_hash_eq_ok_def]
  \\ fs[paramhash_build_m_ok_def]
  \\ fs[paramhash_build_ps_stores_U_fast_atlas_eq_def,
        paramhash_build_stores_U_fast_atlas_eq_def,
        paramhash_build_state_def]
  \\ `set_atlas_eq (set (paramhash_build_ps g))
        (ph_set (ph_build_from_create_state (paramhash_build_m g) (paramhash_build_ps g)))` by
       (match_mp_tac ph_build_from_create_state_set_atlas_eq_set_ps \\ simp[])
  \\ fs[set_atlas_eq_def]
  \\ metis_tac[]
QED

val _ = export_theory ();
